#!/bin/sh
cd $(dirname "$0")/..

python3 \
  scripts/integration.py \
  scripts/integration.txt \
  scripts/integration.rs \
  src/bin/test-integration.rs && \
cargo build --release && \
target/release/test-integration
