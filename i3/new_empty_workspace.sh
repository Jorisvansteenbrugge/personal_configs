#!/bin/bash
NEW_WORKSPACE=$(i3-msg -t get_workspaces | jq 'map(.num) | max + 1')

if [[ $NEW_WORKSPACE == "null" ]]; then
  NEW_WORKSPACE=1
fi

i3-msg workspace number $NEW_WORKSPACE

