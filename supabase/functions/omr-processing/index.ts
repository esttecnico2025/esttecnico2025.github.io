import { createClient } from "npm:@supabase/supabase-js@2";

const corsHeaders = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "authorization, x-client-info, apikey, content-type, x-worker-secret",
  "Access-Control-Allow-Methods": "POST, OPTIONS",
};
const json = (body: unknown, status = 200) => new Response(JSON.stringify(body), {
  status, headers: { ...corsHeaders, "Content-Type": "application/json" },
});
const csvCell = (value: unknown) => {
  const s = value === null || value === undefined ? "" : String(value);
  return '"' + s.replaceAll('"', '""') + '"';
};
const csvText = (rows: unknown[][]) => rows.map(row => row.map(csvCell).join(",")).join("\r\n");
const safeName = (value: string) => value.normalize("NFD").replace(/[\u0300-\u036f]/g, "").replace(/[^A-Za-z0-9_-]+/g, "_").slice(0, 48) || "evaluacion";
const sameMap = (a: any[], b: any[]) => JSON.stringify(a.map(q => [Number(q.session),Number(q.position),String(q.item_code),String(q.area),Number(q.area_order),Number(q.question_in_area),Boolean(q.annulled)])) === JSON.stringify(b.map(q => [Number(q.session),Number(q.position),String(q.item_code),String(q.area),Number(q.area_order),Number(q.question_in_area),Boolean(q.annulled)]));

const url = Deno.env.get("SUPABASE_URL")!;

const secretKeys = JSON.parse(
  Deno.env.get("SUPABASE_SECRET_KEYS")!
);

const serviceKey = secretKeys["default"];

if (!serviceKey) {
  throw new Error("No se encontró la clave secreta predeterminada de Supabase.");
}

const admin = createClient(url, serviceKey, {
  auth: {
    persistSession: false,
    autoRefreshToken: false
  }
});

async function buildInput(ownerId: string, examId: string) {
  const { data: exam, error: examError } = await admin.from("exams")
    .select("id,title,configuration").eq("owner_id", ownerId).eq("id", examId).maybeSingle();
  if (examError) throw examError;
  if (!exam) throw new Error("La evaluación no existe o no pertenece a la cuenta autenticada.");
  const cfg = exam.configuration || {};
  const model = String(cfg.grading_model || "").toLowerCase();
  if (!["regular", "normative", "tri"].includes(model)) throw new Error("La evaluación no tiene un modelo de calificación válido.");
  const settings = cfg.grading_settings || {};
  const triModel = String(settings.irt_model || "").toUpperCase();
  if (model === "tri" && !["2PL", "3PL"].includes(triModel)) throw new Error("TRI requiere selección explícita de 2PL o 3PL.");
  const configuredSessions = (cfg.session_config || []).map((s: any) => Number(s.session)).filter((n: number) => Number.isInteger(n) && n > 0).sort((a: number,b: number)=>a-b);
  if (!configuredSessions.length) throw new Error("La evaluación no tiene sesiones configuradas.");
  if (new Set(configuredSessions).size !== configuredSessions.length) throw new Error("La configuración contiene sesiones duplicadas.");
  const { data: scanRows, error: scansError } = await admin.from("omr_scan_records")
    .select("session_number,record,processed_at").eq("owner_id", ownerId).eq("exam_id", examId)
    .order("session_number", { ascending: true }).order("processed_at", { ascending: true });
  if (scansError) throw scansError;
  if (!scanRows?.length) throw new Error("No hay registros OMR respaldados para esta evaluación.");
  const grouped = new Map<number, any[]>();
  for (const row of scanRows) {
    const s = Number(row.session_number); if (!grouped.has(s)) grouped.set(s, []);
    grouped.get(s)!.push(row.record || {});
  }
  const allFiles: {name:string;content:string}[] = [];
  const uniqueDocuments = new Set<string>();
  for (const session of configuredSessions) {
    const records = grouped.get(session) || [];
    if (!records.length) throw new Error(`La sesión ${session} no tiene registros respaldados. No se ejecutará un consolidado parcial.`);
    const invalid = records.filter(r => ["Revisión", "Error"].includes(String(r.status || "")));
    if (invalid.length) throw new Error(`La sesión ${session} tiene ${invalid.length} registro(s) en Revisión/Error. Corrígelos antes de procesar.`);
    const eligible = records.filter(r => String(r.codigo || "").trim() && Array.isArray(r.results));
    if (eligible.length !== records.length) throw new Error(`La sesión ${session} contiene registros sin código o sin respuestas binarias.`);
    const first = eligible[0];
    if (!Array.isArray(first.questionMap) || !first.questionMap.length || first.questionMapSource === "unavailable") {
      throw new Error(`La sesión ${session} no tiene un mapa histórico guardado en sus registros. No se reconstruirá desde la configuración actual.`);
    }
    const map = [...first.questionMap].sort((a,b)=>Number(a.position)-Number(b.position));
    if (map.length !== first.results.length) throw new Error(`La sesión ${session}: el mapa no coincide con la cantidad de respuestas.`);
    map.forEach((q,i)=>{
      if (Number(q.session)!==session || Number(q.position)!==i+1 || !String(q.item_code||"").trim() || !String(q.area||"").trim() || !Number.isInteger(Number(q.area_order)) || !Number.isInteger(Number(q.question_in_area))) {
        throw new Error(`La sesión ${session}: mapa histórico inválido en la posición ${i+1}.`);
      }
    });
    const codes = new Set(map.map(q=>String(q.item_code)));
    if(codes.size!==map.length) throw new Error(`La sesión ${session}: hay códigos de pregunta duplicados en el mapa.`);
    const seen = new Set();
    for (const rec of eligible) {
      if (String(rec.examId || examId)!==examId || Number(rec.session)!==session) throw new Error(`Hay un registro que no corresponde a la evaluación/sesión ${session}.`);
      if (!Array.isArray(rec.questionMap) || !sameMap(map,[...rec.questionMap].sort((a,b)=>Number(a.position)-Number(b.position))) || rec.results.length!==map.length) {
        throw new Error(`El registro ${rec.codigo} tiene un mapa distinto dentro de la sesión ${session}. No se mezclará con los demás.`);
      }
      const code=String(rec.codigo).trim();if(seen.has(code))throw new Error(`Documento duplicado en sesión ${session}: ${code}.`);seen.add(code);
      for(let i=0;i<rec.results.length;i++){
        const v=rec.results[i];if(v!==0&&v!==1&&v!==null&&v!==undefined)throw new Error(`Respuesta binaria inválida en ${code}, sesión ${session}, pregunta ${i+1}.`);
        if(v===null&&!Boolean(map[i].annulled))throw new Error(`El registro ${code} marca como anulada una pregunta que no figura anulada en su mapa histórico.`);
      }
    }
    for (const rec of eligible) uniqueDocuments.add(String(rec.codigo).trim());
    const answerHeader = ["documento", ...map.map((_:any,i:number)=>`P${i+1}`)];
    const answerRows: unknown[][] = [answerHeader];
    for (const rec of eligible) {
      answerRows.push([String(rec.codigo).trim(), ...map.map((q:any,i:number)=>Boolean(q.annulled)?"NA":(rec.results[i]===1?1:0))]);
    }
    const mapHeader = ["evaluacion_id","evaluacion","modelo_calificacion","sesion","posicion_respuesta","codigo_pregunta","area","orden_area","pregunta_en_area","anulada","metodo_normativo","referencia_normativa","media_area","sd_area","media_global","sd_global","modelo_tri","tri_media","tri_sd","tri_min","tri_max"];
    const mapRows: unknown[][] = [mapHeader];
    for (const q of map) {
      mapRows.push([exam.id,exam.title||"Evaluación",model,session,Number(q.position),q.item_code,q.area,Number(q.area_order),Number(q.question_in_area),Boolean(q.annulled),
        model==="normative"?"rasch_unal":"",model==="normative"?"cohorte_evaluada":"",
        Number(settings.component_mean??10),Number(settings.component_sd??1),Number(settings.global_mean??500),Number(settings.global_sd??100),
        model==="tri"?triModel:"",Number(settings.scale_mean??50),Number(settings.scale_sd??18),Number(settings.scale_min??0),Number(settings.scale_max??100)]);
    }
    const suffix=safeName(String(exam.id));
    allFiles.push({name:`respuestas_omr_${suffix}_sesion_${session}.csv`,content:csvText(answerRows)});
    allFiles.push({name:`mapa_preguntas_sesion_${session}.csv`,content:csvText(mapRows)});
  }
  // No se procesan sesiones adicionales que no estén declaradas en la configuración.
  const unexpected=[...grouped.keys()].filter(s=>!configuredSessions.includes(s));
  if(unexpected.length)throw new Error("Hay registros de sesiones no configuradas: "+unexpected.join(", "));
  return {model,totalStudents:uniqueDocuments.size,sessions:configuredSessions,input_payload:{files:allFiles,model,exam_id:examId}};
}

Deno.serve(async (req) => {
  if (req.method === "OPTIONS") return new Response("ok", { headers: corsHeaders });
  if (req.method !== "POST") return json({error:"Método no permitido."},405);
  let body:any;try{body=await req.json();}catch{return json({error:"JSON inválido."},400);}
  const action=String(body.action||"");
  const workerSecret=Deno.env.get("OMR_WORKER_SECRET")||"";
  const workerAction=["worker-get","worker-complete","worker-fail"].includes(action);
  if(workerAction){
    const supplied=req.headers.get("x-worker-secret")||"";
    if(!workerSecret||supplied!==workerSecret)return json({error:"No autorizado."},401);
    const jobId=String(body.job_id||"");if(!/^[0-9a-f-]{36}$/i.test(jobId))return json({error:"job_id inválido."},400);
    if(action==="worker-get"){
      const {data,error}=await admin.from("omr_processing_jobs").select("id,input_payload,status").eq("id",jobId).maybeSingle();
      if(error)throw error;if(!data)return json({error:"Trabajo no encontrado."},404);
      if(!["queued","running"].includes(data.status))return json({error:"El trabajo ya no está en cola.",status:data.status},409);
      const {error:updateError}=await admin.from("omr_processing_jobs").update({status:"running",started_at:new Date().toISOString(),updated_at:new Date().toISOString()}).eq("id",jobId);
      if(updateError)throw updateError;
      return json({job_id:jobId,input_payload:data.input_payload});
    }
    if(action==="worker-complete"){
      const result=body.result_payload;if(!result||!Array.isArray(result.results)||!Array.isArray(result.detail)||!Array.isArray(result.audit))return json({error:"Resultado incompleto."},400);
      const {error}=await admin.from("omr_processing_jobs").update({status:"completed",result_payload:result,error_message:null,completed_at:new Date().toISOString(),updated_at:new Date().toISOString()}).eq("id",jobId);
      if(error)throw error;return json({ok:true});
    }
    const message=String(body.error_message||"Falló el workflow de GitHub Actions.").slice(0,4000);
    const {error}=await admin.from("omr_processing_jobs").update({status:"failed",error_message:message,completed_at:new Date().toISOString(),updated_at:new Date().toISOString()}).eq("id",jobId);
    if(error)throw error;return json({ok:true});
  }
  if(action!=="start")return json({error:"Acción no reconocida."},400);
  const auth=req.headers.get("authorization")||"";const token=auth.replace(/^Bearer\s+/i,"");
  if(!token)return json({error:"Inicia sesión para procesar."},401);
  const {data:userData,error:userError}=await admin.auth.getUser(token);
  if(userError||!userData?.user)return json({error:"Sesión no válida."},401);
  const examId=String(body.exam_id||"");if(!/^[0-9a-f-]{36}$/i.test(examId))return json({error:"Identificador de evaluación inválido."},400);
  let createdJobId:string|null=null;
  try{
    const {data:activeJob,error:activeError}=await admin.from("omr_processing_jobs").select("id,status").eq("owner_id",userData.user.id).eq("exam_id",examId).in("status",["queued","running"]).order("created_at",{ascending:false}).limit(1).maybeSingle();
    if(activeError)throw activeError;
    if(activeJob)return json({job_id:activeJob.id,status:activeJob.status,already_running:true});
    const built=await buildInput(userData.user.id,examId);
    const {data:job,error:insertError}=await admin.from("omr_processing_jobs").insert({owner_id:userData.user.id,exam_id:examId,model:built.model,status:"queued",total_students:built.totalStudents,sessions:built.sessions,input_payload:built.input_payload}).select("id").single();
    if(insertError)throw insertError;
    createdJobId=job.id;
    const owner=Deno.env.get("GITHUB_OWNER")||"esttecnico2025";
    const repo=Deno.env.get("GITHUB_REPO")||"esttecnico2025.github.io";
    const ref=Deno.env.get("GITHUB_REF")||"main";
    const workflow=Deno.env.get("GITHUB_WORKFLOW_FILE")||"omr-processing.yml";
    const githubToken=Deno.env.get("GITHUB_TOKEN")||"";
    if(!githubToken)throw new Error("Falta configurar el secreto GITHUB_TOKEN en Supabase.");
    const dispatch=await fetch(`https://api.github.com/repos/${owner}/${repo}/actions/workflows/${workflow}/dispatches`,{
      method:"POST",headers:{"Accept":"application/vnd.github+json","Authorization":`Bearer ${githubToken}`,"X-GitHub-Api-Version":"2022-11-28","Content-Type":"application/json"},
      body:JSON.stringify({ref,inputs:{job_id:job.id}})
    });
    if(!dispatch.ok){const detail=await dispatch.text();throw new Error(`GitHub rechazó el trabajo (${dispatch.status}): ${detail.slice(0,900)}`);}
    return json({job_id:job.id,status:"queued",model:built.model,sessions:built.sessions,total_students:built.totalStudents});
  }catch(err){
    console.error("omr-processing start:",err);
    if(createdJobId){
      await admin.from("omr_processing_jobs").update({status:"failed",error_message:String(err?.message||err).slice(0,4000),completed_at:new Date().toISOString(),updated_at:new Date().toISOString()}).eq("id",createdJobId);
    }
    return json({error:err?.message||"No se pudo iniciar el procesamiento."},400);
  }
});
