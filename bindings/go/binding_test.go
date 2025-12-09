package tree_sitter_alex_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_alex "github.com/tree-sitter/tree-sitter-alex/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_alex.Language())
	if language == nil {
		t.Errorf("Error loading Alex grammar")
	}
}
