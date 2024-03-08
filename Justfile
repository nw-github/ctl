clean:
    rm main.c
    rm a.out
    rm perf.data*
    rm flamegraph.svg

code:
    cd ide/code && npm yaml-transpile
    cd ide/code && rsync -av . ~/.vscode-oss/extensions/ctl --exclude node_modules --exclude .vscode
