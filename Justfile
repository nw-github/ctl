clean:
    rm main.c
    rm a.out

code:
    cd ide/code && just yaml
    cd ide/code && rsync -av . ~/.vscode-server/extensions/ctl --exclude node_modules --exclude .vscode
