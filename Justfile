clean:
    rm main.c
    rm a.out
    rm perf.data*
    rm flamegraph.svg

code:
    cd ide/code && npm run compile && rsync -av . ~/.vscode-oss/extensions/ctl
