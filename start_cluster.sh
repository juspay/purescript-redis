#!/bin/bash

set -e

CLUSTER_DIR="$(dirname ${0})/test/redis-cluster"
NODE_COUNT=6

if [ ! -e ${CLUSTER_DIR} ]; then
  mkdir ${CLUSTER_DIR}
fi

cd ${CLUSTER_DIR}

PIDS=()

CLUSTER_CREATE="redis-cli --cluster create"

for i in $(seq 1 ${NODE_COUNT}); do
  NODE_PORT=$((${i} + 6999))

  if [ ! -e ${NODE_PORT} ]; then
    mkdir ${NODE_PORT}
  fi

  cd ${NODE_PORT}

  cat > redis.conf <<EOF
port ${NODE_PORT}
cluster-enabled yes
cluster-config-file nodes.conf
cluster-node-timeout 5000
appendonly yes
EOF

  redis-server ./redis.conf &

  cd ..

  CLUSTER_CREATE="${CLUSTER_CREATE} 127.0.0.1:${NODE_PORT}"
done

CLUSTER_CREATE="${CLUSTER_CREATE} --cluster-replicas 1"

${CLUSTER_CREATE}
