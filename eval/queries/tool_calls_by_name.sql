-- tool_calls_by_name.sql
--
-- Count and total-duration of each tool the agent called, one row
-- per tool name. Answers "what did the agent actually spend its
-- time on."
--
-- Usage:
--   trace_processor_shell -q eval/queries/tool_calls_by_name.sql \
--     eval/traces/<pr>/hutch-trace-*.json

SELECT
  name                       AS tool,
  COUNT(*)                   AS calls,
  ROUND(SUM(dur) / 1e6, 1)   AS total_ms
FROM slice
WHERE category = 'tool'
GROUP BY name
ORDER BY total_ms DESC;
