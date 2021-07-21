#!/usr/bin/env dub
/+dub.sdl:
    name "remove_comments"
    description "For single-file scripts, we can embed a dub.sdl, and call it with `dub run --single my_script.d`!"
+/

import std; // Import the entire standard library

void main()
{
    const contents = readText("source/app.d");
    auto output = File("source/app_without_comments.d", "w");
    
    contents
        .splitter('\n')
        .filter!(line => !line.strip.startsWith("//"))
        .each!(line => output.writeln(line));
}