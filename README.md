# Marios Control Program

A sort of uGHCi with bells and whistles preconfigured to work as a shell.

## Features

* Background jobs (forkIO) have access to the externalPrint function from haskeline,
  allowing output to the console without disturbing the prompt.
* Host executables all available qualified in the OS module.
* Simple support for changing the current directory without trashing
  the context.
* Simple expression completion (nothing GHCi can't also do).
* Integrated internet radio player with current title reporting via
  externalPrint.
* Direct rendering of diagrams to console via diagrams-braille.

All this project does is bring together some very nice Haskell libraries.
There is really no novelty here.  All the hard work has been
done by the maintainers/authors of the packages this project depends on.

## Dependencies

MCP uses GStreamer to play background music.

```console
# apt install libgirepository1.0-dev libgstreamer1.0-dev
```

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
% glob "lib/*.hs"
["lib/Ops.hs","lib/Radio.hs","lib/OS.hs"]
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

### Drawing diagrams

```console
% draw $ circle 1 <> square (sqrt 2)
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⡤⠴⠒⠛⠉⠉⠉⠉⠉⠉⠉⠉⠛⠒⠦⢤⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⣠⠖⠋⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠙⠲⣄⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⢀⣴⣋⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣙⣦⡀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⡴⢻⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡟⢦⠀⠀⠀⠀
⠀⠀⢠⠞⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠳⡄⠀⠀
⠀⢠⠏⠀⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠹⡄⠀
⢀⡏⠀⠀⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⢹⡀
⣼⠀⠀⠀⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⣧
⡇⠀⠀⠀⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⢸
⡇⠀⠀⠀⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⢸
⡇⠀⠀⠀⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⢸
⡇⠀⠀⠀⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⢸
⢻⠀⠀⠀⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⠀⡟
⠈⣇⠀⠀⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⠀⣸⠁
⠀⠘⣆⠀⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⠀⣰⠃⠀
⠀⠀⠘⢦⠀⢸⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡇⠀⡴⠃⠀⠀
⠀⠀⠀⠀⠳⣼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣧⠞⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠈⠻⣍⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⣩⠟⠁⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠙⠦⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⠴⠋⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠓⠲⠤⣤⣀⣀⣀⣀⣀⣀⣀⣀⣤⠤⠖⠚⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
```

