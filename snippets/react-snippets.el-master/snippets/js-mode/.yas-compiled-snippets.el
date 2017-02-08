;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
										 '(("ss" "this.setState({$0});\n" "React#setState" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/ss.yasnippet" nil nil)
											 ("sp" "this.setProps({$0});\n" "React#setProps" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/sp.yasnippet" nil nil)
											 ("scu" "shouldComponentUpdate: function(${1:nextProps}, ${2:nextState}) {\n  $0\n}\n" "React#shouldComponentUpdate" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/scu.yasnippet" nil nil)
											 ("rr" "React.render(\n  $1,\n  $0\n);\n" "React.render" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/rr.yasnippet" nil nil)
											 ("r" "render: function() {\n  $0\n}\n" "React#render" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/r.yasnippet" nil nil)
											 ("gis" "getInitialState: function() {\n  $0\n}\n" "React#getInitialState" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/gis.yasnippet" nil nil)
											 ("gdp" "getDefaultProps: function() {\n  $0\n}\n" "React#getDefaultProps" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/gdp.yasnippet" nil nil)
											 ("cwum" "componentWillUnmount: function() {\n  $0\n}\n" "React#componentWillUnmount" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/cwum.yasnippet" nil nil)
											 ("cwu" "componentWillUpdate: function(${1:nextProps}, ${2:nextState}) {\n  $0\n}\n" "React#componentWillUpdate" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/cwu.yasnippet" nil nil)
											 ("cwrp" "componentWillReceiveProps: function(${1:nextProps}) {\n  $0\n}\n" "React#componentWillReceiveProps" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/cwrp.yasnippet" nil nil)
											 ("cwm" "componentWillMount: function() {\n  $0\n}\n" "React#componentWillMount" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/cwm.yasnippet" nil nil)
											 ("cdu" "componentDidUpdate: function(${1:prevProps}, ${2:prevState}, ${3:rootNode}) {\n  $0\n}\n" "React#componentDidUpdate" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/cdu.yasnippet" nil nil)
											 ("cdm" "componentDidMount: function(${1:rootNode}) {\n  $0\n}\n" "React#componentDidMount" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/cdm.yasnippet" nil nil)
											 ("cc" "React.createClass({\n  $0\n});\n" "React.createClass" nil nil nil "/Users/gcperrin/.dotfiles/snippets/react-snippets.el-master/snippets/js-mode/cc.yasnippet" nil nil)))


;;; Do not edit! File generated at Mon Feb  6 11:38:35 2017
