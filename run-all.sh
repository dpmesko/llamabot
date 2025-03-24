#!/bin/bash

set -e

TOKEN=$(cat ./token)
LOGS_DIR='./logs'



echo "killing existing executables if running"

OLD_APP_PID=$(awk 'NR == 1 {print $3}' $LOGS_DIR/pids.txt)
OLD_SLASH_COMMAND_PID=$(awk 'NR == 2 {print $4}' $LOGS_DIR/pids.txt)
kill "$OLD_APP_PID"
kill "$OLD_SLASH_COMMAND_PID"

cd ./scripts



./install_deps.sh
./build.sh

cd ../

if [ ! -d "$LOGS_DIR" ]; then
  mkdir "$LOGS_DIR"
fi

echo "killing existing executables if running"
OLD_APP_PID=(awk 'NR == 0 {print $2}' $LOGS_DIR/pids.txt)
echo "$OLD_APP_PID"

set +e
echo "starting executables"

./scripts/executables/llamabot --token=$TOKEN --port=8081 >>$LOGS_DIR/app.log &

APP_PID=$!

./scripts/executables/llamabot-slash-command --token=$TOKEN --port=8082 >>$LOGS_DIR/slash-command.log &

SLASH_COMMAND_PID=$!

echo "executables started, writing PIDs to $LOGS_DIR/pids.txt"
echo "APP PID: $APP_PID" > $LOGS_DIR/pids.txt
echo "SLASH COMMAND PID: $SLASH_COMMAND_PID" >> $LOGS_DIR/pids.txt
