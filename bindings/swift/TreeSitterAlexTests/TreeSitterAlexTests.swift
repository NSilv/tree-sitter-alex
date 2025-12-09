import XCTest
import SwiftTreeSitter
import TreeSitterAlex

final class TreeSitterAlexTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_alex())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Alex grammar")
    }
}
