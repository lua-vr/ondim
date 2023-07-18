# Revision history for ondim

## 0.1.0.0 -- 2023-07-18

* First version. You should consider most of user-facing template language as experimental and unstable, especially Pandoc and LaTeX targets. HTML templates are the most tested and stable. The overall Haskell library API is relatively stable. Documentation is missing for most modules.
* Language support: HTML, JSON, Whiskers, Pandoc and LaTeX targets.
* Main template engine features: 
  - No special syntax: templates are interpreted inside existing languages.
  - Built around the concept of expansions and textual data, which are organized into namespaces.
  - Helpful error message system that display traces and definition sites.
  - Work with multiple languages at once: you can have HTML templates inside the text of JSON templates, for instance, and they will share the same environment.
* Haskell library features:
  - Easy to support new languages with generics: existing ASTs and parsers can be reused.
  - Write polymorphic template code, "once and for all": most functions will work for all target languages. Write type-specific code when you need it.
  - `Ondim.Extra.Loading`: helper for loading templates from disk. You should check the language-specific loader configurations in `Ondim.Targets.*.Load` modules.
  - `Ondim.Extra.BindJSON`: module for exposing JSON data to templates.
  - `Ondim.Extra.Expansions`: Functions for exposing Haskell lists, maps and booleans to templates.
  - `Ondim.Extra.Standard`: a small (optional) standard library of expansions for control flow and environment modification.
