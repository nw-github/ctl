VSCODE_HOME := if path_exists(home_directory() / ".vscode-server") == "true" {
    home_directory() / ".vscode-server"
} else if path_exists(home_directory() / ".vscode-oss") == "true" {
    home_directory() / ".vscode-oss"
} else {
    home_directory() / ".vscode"
}

clean:
    rm main.c
    rm a.out
    rm perf.data*
    rm flamegraph.svg

sync:
    git pull
    just code
    cargo b -r

code:
    cd ide/code && npm install && npm run compile && rsync -av . {{VSCODE_HOME}}/extensions/ctl
