-- Requisito previo de OMR Vision Suite: almacenamiento de hojas OMR respaldadas.
-- Si public.omr_scan_records ya existe y funciona, NO vuelvas a ejecutar este archivo.
create table if not exists public.omr_scan_records (
  id uuid primary key default gen_random_uuid(),
  owner_id uuid not null references auth.users(id) on delete cascade,
  exam_id uuid not null references public.exams(id) on delete cascade,
  student_id uuid not null references public.students(id) on delete cascade,
  session_number integer not null check (session_number > 0),
  record jsonb not null,
  processed_at timestamptz not null default now(),
  created_at timestamptz not null default now(),
  constraint omr_scan_records_owner_exam_student_session_key
    unique (owner_id, exam_id, student_id, session_number)
);
create index if not exists omr_scan_records_owner_processed_idx
  on public.omr_scan_records (owner_id, processed_at desc);
alter table public.omr_scan_records enable row level security;
drop policy if exists "Users manage own omr_scan_records" on public.omr_scan_records;
create policy "Users manage own omr_scan_records"
  on public.omr_scan_records for all
  using (owner_id = auth.uid())
  with check (owner_id = auth.uid());
