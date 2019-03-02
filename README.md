# Marios Control Program

## Examples

### Changing directories

```console
% getCurrentDirectory
"/home/mlang/src/mcp"
% cd
% getCurrentDirectory
"/home/mlang"
% cd "src/mcp"
% getCurrentDirectory
"/home/mlang/src/mcp"
```

### Globbing

```console
% OS.ls
app  lib  LICENSE  mcp.cabal  package.yaml  Setup.hs  stack.yaml
% OS.ls =<< glob "lib/*.hs"
lib/Ops.hs  lib/OS.hs  lib/Radio.hs
```

### Threads and Variables

```console
% let watch r p = forkIO . forever $ printProc p >> readIORef r >>= OS.sleep
% delay <- newIORef 10
% clock <- watch delay OS.date
Sat Mar  2 21:32:28 CET 2019
Sat Mar  2 21:32:38 CET 2019
% writeIORef delay 5
Sat Mar  2 21:32:48 CET 2019
Sat Mar  2 21:32:53 CET 2019
Sat Mar  2 21:32:58 CET 2019
Sat Mar  2 21:33:03 CET 2019
% killThread clock
```

