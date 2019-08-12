c.tabs.position = "left"

# Ctrl-c as Escape
c.bindings.key_mappings['<Ctrl-c>'] = '<Escape>'
config.bind('<Ctrl-c>', 'leave-mode', mode="command")

config.bind(',p', 'spawn --userscript qute-lastpass')
config.bind(',s', 'config-clear ;; config-source ;; message-info "configuration reloaded!"')
config.bind(',qq', 'quit')
config.bind('Ctrl-p', 'set-cmd-text -s :bookmark-load -t');
