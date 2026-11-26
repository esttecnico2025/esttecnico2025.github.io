#!/usr/bin/env python3
import csv, json, pathlib, re, sys
folder=pathlib.Path(sys.argv[1] if len(sys.argv)>1 else "work/output")
def scalar(v):
    if v is None or v=="": return None
    s=str(v).strip()
    if re.fullmatch(r"[-+]?\d+",s):
        try:return int(s)
        except:return s
    if re.fullmatch(r"[-+]?(?:\d+\.\d*|\.\d+)(?:[eE][-+]?\d+)?",s):
        try:return float(s)
        except:return s
    return s
def read_suffix(suffix):
    files=list(folder.glob("*"+suffix+".csv"))
    if len(files)!=1: raise SystemExit(f"Se esperaba exactamente un archivo *{suffix}.csv; encontrados: {len(files)}")
    with files[0].open(encoding="utf-8-sig",newline="") as f:
        return [{k:scalar(v) for k,v in row.items()} for row in csv.DictReader(f)]
result={"results":read_suffix("_resultados"),"detail":read_suffix("_detalle"),"audit":read_suffix("_auditoria_items"),"motor_version":"1.0.1"}
target=pathlib.Path("work/result_payload.json")
target.parent.mkdir(parents=True,exist_ok=True)
target.write_text(json.dumps(result,ensure_ascii=False,separators=(",",":")),encoding="utf-8")
print("Resultados convertidos a JSON:",len(result["results"]),"estudiantes;",len(result["detail"]),"filas de detalle;",len(result["audit"]),"ítems de auditoría")
