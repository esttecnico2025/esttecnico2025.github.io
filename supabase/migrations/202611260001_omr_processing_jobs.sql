-- OMR Vision Suite: cola segura de trabajos R ejecutados en GitHub Actions.
-- Ejecutar una sola vez en Supabase > SQL Editor.
create table if not exists public.omr_processing_jobs (
  id uuid primary key default gen_random_uuid(),
  owner_id uuid not null references auth.users(id) on delete cascade,
  exam_id uuid not null references public.exams(id) on delete cascade,
  model text not null check (model in ('regular','normative','tri')),
  status text not null default 'queued' check (status in ('queued','running','completed','failed')),
  total_students integer not null default 0 check (total_students >= 0),
  sessions integer[] not null default '{}',
  input_payload jsonb not null,
  result_payload jsonb,
  error_message text,
  created_at timestamptz not null default now(),
  started_at timestamptz,
  completed_at timestamptz,
  updated_at timestamptz not null default now()
);
create index if not exists omr_processing_jobs_owner_exam_created_idx
  on public.omr_processing_jobs(owner_id, exam_id, created_at desc);
create index if not exists omr_processing_jobs_status_created_idx
  on public.omr_processing_jobs(status, created_at);
alter table public.omr_processing_jobs enable row level security;
drop policy if exists "Users read own omr_processing_jobs" on public.omr_processing_jobs;
create policy "Users read own omr_processing_jobs"
  on public.omr_processing_jobs for select
  using (owner_id = auth.uid());
revoke all on public.omr_processing_jobs from anon;
grant select on public.omr_processing_jobs to authenticated;
-- No se concede INSERT/UPDATE/DELETE al navegador. La Edge Function usa service_role.
