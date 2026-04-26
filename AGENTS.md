# AGENTS.md


## Instruction
Check `ROADMAP.md` and `rua-design.md` for the overall design and implementation plan. You can also check `notes.md` for the development notes and progress updates.

`lua5.5.txt` contains the complete Lua 5.5 reference manual. It's very big. To prevent polluting your context, only read the relevant sections that you need.

`tests/lua-upstream-tests` contains the official Lua test suite. You should use these tests to verify your implementation. 

After each session, summarize your work and append them to `design_notes/notes.md`. There is an anchor `## APPEND HERE` at the end of file. You can use replace tool to append your summary **before** the anchor without reading the file. The anchor must remain at the end of the file.


## Debugging
Follow this procedure to debug:
- You can use `dbg!` macro or `rust-gdb` (if on linux) to debug your code.
- Do not propose case by case fix.
- If a fix is too complicated, always ask for user's help to find a better solution. You can ask user to provide more information, or you can ask user to try some code snippets to see if they work.
- After you fix the bug, if possible, write a regression test to prevent the same bug from happening again. You can put the test Lua files in `tests/` directory.