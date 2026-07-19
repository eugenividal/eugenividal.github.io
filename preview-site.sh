#!/usr/bin/env bash

set -e

PORT="${1:-4200}"

cleanup() {
  echo
  echo "Stopping website preview..."
  kill "$EN_PID" "$CA_PID" "$SERVER_PID" 2>/dev/null || true
}

trap cleanup EXIT INT TERM

echo "Rendering both language versions..."

quarto render
(
  cd ca
  quarto render
)

echo "Watching English files..."
quarto preview --no-serve --no-browser --no-navigate &
EN_PID=$!

echo "Watching Catalan files..."
(
  cd ca
  quarto preview --no-serve --no-browser --no-navigate
) &
CA_PID=$!

echo "Serving the complete website at:"
echo "http://localhost:${PORT}"
echo
echo "Press Ctrl+C to stop."

python3 -m http.server "$PORT" --bind 127.0.0.1 &
SERVER_PID=$!

wait

