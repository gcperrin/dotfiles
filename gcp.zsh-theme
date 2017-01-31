local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )"
PROMPT='%{$FG[085]%}%n%{$FG[159]%}@%{$FG[085]%}%m%{$FG[105]%}[%1~]%{$reset_color%} $(git_prompt_info)'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$FG[251]%}(%{$FG[001]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$FG[251]%})%{$FG[160]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$FG[251]%})"
