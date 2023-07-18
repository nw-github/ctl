run path:
    cargo -q r -- {{path}} | clang -std=c11 -lgc -I./ctl -x c - && ./a.out

print path:
    cargo -q r -- {{path}} | clang-format

clean:
    rm main.c
    rm a.out

code:
    cd ide/code && just yaml
    cd ide/code && rsync -av . ~/.vscode-oss/extensions/ctl --exclude node_modules --exclude .vscode
