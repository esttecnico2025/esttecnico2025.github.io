#!/usr/bin/env python3
import json, pathlib, re, sys
source=pathlib.Path(sys.argv[1] if len(sys.argv)>1 else "work/job.json")
out=pathlib.Path(sys.argv[2] if len(sys.argv)>2 else "work/input")
data=json.loads(source.read_text(encoding="utf-8"))
payload=data.get("input_payload",data)
files=payload.get("files")
if not isinstance(files,list) or not files: raise SystemExit("El trabajo no contiene archivos CSV.")
out.mkdir(parents=True,exist_ok=True)
allowed=re.compile(r"^(respuestas_omr_[A-Za-z0-9_-]+_sesion_[0-9]+|mapa_preguntas_sesion_[0-9]+)\.csv$")
for item in files:
    name=str(item.get("name", "")); content=item.get("content")
    if not allowed.fullmatch(name) or not isinstance(content,str): raise SystemExit(f"Archivo de entrada no permitido: {name}")
    (out/name).write_text(content,encoding="utf-8",newline="")
print(f"Preparados {len(files)} CSV en {out}")
