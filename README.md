# hiedb-plugin

A GHC plugin which automatically re-indexes recompiled hie files into an [hiedb](https://github.com/wz1000/hiedb) SQLite database.

To use this plugin:

- add the `hiedb-plugin` package as a build dependency of your package

- add `ghc-options: -plugin-package hiedb-plugin -fplugin Plugin.Hiedb` to your package

Properties:
- Requires `-hiedir` to be set.
- Will index to `.hiedb`. Will likely be configurable in the futrue
- Currently skips type indexing for performance reasons. Will likely be
  configurable in the future
