# Covariates — sources and verification status

- `itpp_serie_2014_2024_wide.csv` — CIPPEC, Índice de Transparencia Presupuestaria
  Provincial, Edición 2024, **Tabla 9** ("Puntaje de serie de tiempo generales y por
  provincia (2014-2024)", p. 19; Szenkman, Navaridas, Cabrera & Gómez). Homogeneous
  time series (0-10), NOT comparable with the annual scores (Tabla 5/10) reported
  in press. Transcribed manually from the PDF 2026-08-27. STATUS: transcribed from
  primary source; spot-check advisable before publication.
- `intra_2024.csv` — Poder Ciudadano + Ruido, Índice Nacional de Transparencia
  (INTRA), relevamiento oct-2024 (publ. 09-12-2024). Only the four NEA values were
  verified from the published report/press; the remaining provinces require the
  Drive spreadsheets (indicetransparencia.ar). STATUS: partial, verified.
- `regimen_provincial.csv` — own provisional coding from public record (governors,
  parties, national alignment by presidential era). `alineado`: 1 = aligned with
  national government, 0.5 = dialoguista/provincial party, 0 = opposition.
  `partido_en_poder_desde`: year the currently governing party/coalition took the
  governorship continuously. STATUS: verified 29-08-2026 — all 72 rows checked
  against the public record (governors, parties, and the eight 2023 turnovers:
  Chaco, Chubut, Entre Ríos, Neuquén, San Juan, San Luis, Santa Cruz, Santa Fe);
  one label corrected (Poggi's 2023 front was "Cambia San Luis"; "Ahora San
  Luis" is the 2025 relabel). Milei-era `alineado` values are judgment calls on
  a fluid object; the five debatable codings (Entre Ríos 1, Mendoza 1, Misiones
  0.5, Salta 0.5, Tucumán 0.5) are tested as a block in the sensitivity table
  `tab_control_politico_sens.csv` (script 31) and do not move the null.
  Alternancia counts still only coded for NEA (unused).
- `uoc_sedes_manual.csv` — hand verifications and corrections for the seats of
  the 539 purchasing units (gazetteer built by `33_uoc_gazetteer.py`; layers:
  DeepSeek prefill, georef localities, Nominatim). Each row cites its source
  (institutional pages, Boletín Oficial, Wikipedia unit articles corroborated,
  the GN squadron deployment annex, or the unit's own denomination). 109 rows,
  10 prefill corrections (e.g. II Brigada Blindada → Paraná; XI Brigada
  Mecanizada → Río Gallegos; Dirección de Remonta y Veterinaria → CABA;
  Hospital Carrillo → Torres; 13° Distrito DNV → Trelew; five GN squadrons to
  the annex seats). STATUS: verified 29-08-2026 for the mapped set (87.6% of
  awards); the unmapped remainder (12.4%) is excluded from Figure 2b and
  declared in its caption.
