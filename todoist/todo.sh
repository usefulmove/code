#!/bin/bash

# add task argument as task to inbox using Todoist REST API

DATA='{"content": "'
DATA+="$1"
DATA+='", "project_id": 2244893558}'

COMMAND='curl "https://api.todoist.com/rest/v1/tasks" -X POST --data '"'"''
COMMAND+=$DATA
COMMAND+=''"'"' -H "Content-Type: application/json" -H "X-Request-Id: $(uuidgen)" -H "Authorization: Bearer c0e02a566b8388261b77718ef2908c55cc774a81"'

eval $COMMAND
