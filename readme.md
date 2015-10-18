# TCP Chat
### compile
```bash
erlc -o out src\server.erl
```
### run
```bash
erl -pa ./out -noshell -run server listen
```
### use
|command|use|
|---|---|
|\help|shows help (a list of commands)|
|\greetme|says "hi" |
|\name NAME|adds "NAME:" to every message sent via this connection|