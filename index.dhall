let List/map = https://prelude.dhall-lang.org/List/map

let items = ./dhall/test-data/dark-souls-items.dhall

let types = ./types.dhall

let Item = { desc : Text, effect : Text, name : Text } : Type

let itemToPost =
      λ(item : Item) →
        let content = item.desc

        let title = item.name

        let filename = "fake-filename"

        let tags = [ item.effect ] : List Text

        let authors = [ "FROM SOFTWARE" ] : List Text

        in  { content, title, filename, tags, authors } : types.BlogPost

in  { posts = List/map Item types.BlogPost itemToPost items } : types.Blog
