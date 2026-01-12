VSCODE_HOME := if path_exists(home_directory() / ".vscode-server") == "true" {
    home_directory() / ".vscode-server"
} else if path_exists(home_directory() / ".vscode-oss") == "true" {
    home_directory() / ".vscode-oss"
} else {
    home_directory() / ".vscode"
}

test:
    cargo test || true
    cargo test --test unit_tests -- run_unit_tests --exact --show-output

clean:
    rm -rf build
    rm perf.data*
    rm flamegraph.svg

sync:
    git pull
    just code
    cargo b -r

code:
    cd ide/code && npm install && npm run compile && rsync -av . {{VSCODE_HOME}}/extensions/ctl
