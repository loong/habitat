#!/bin/bash
cd ~/Workspaces/go/src/github.com/mindworker/go-plainexchange

tmux new -d -s 'mush'
tmux split-window -d -t 0 -h

tmux send-keys -t 0 './run.sh' enter
tmux send-keys -t 1 'heroku pg:psql' enter

tmux new-window 'emacs -nw app/controllers/views.go'

tmux attach -t 'mush'
