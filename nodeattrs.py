ALT_NODE_ATTR = "__alt"
STMT_NODE_ATTR = "__stmt"
BLOCK_START_NODE_ATTR = "__start_block"
REWRITTEN_NODE_ATTR = "__rewritten"


# formatting directives
NEWLINE_NODE_ATTR = "__newline"
INDENT_INCR_NODE_ATTR = "__indent_incr"
INDENT_DECR_NODE_ATTR = "__indent_decr"
INDENT_AROUND_NODE_ATTR = "__indent_around"


ALL_SETTABLE_ATTRS = (STMT_NODE_ATTR,
                      BLOCK_START_NODE_ATTR,
                      REWRITTEN_NODE_ATTR,
                      NEWLINE_NODE_ATTR,
                      INDENT_AROUND_NODE_ATTR,
                      INDENT_INCR_NODE_ATTR,
                      INDENT_DECR_NODE_ATTR,)
