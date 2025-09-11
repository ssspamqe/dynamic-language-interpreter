package ru.innopolis.interpreter;

import org.junit.jupiter.api.Test;
import ru.innopolis.interpeter.RegexLexer;
import ru.innopolis.interpeter.lexer.tokens.Token;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class RegexLexerTest {

    private final RegexLexer lexer = new RegexLexer();

    @Test
    void runAllExampleTests() throws IOException {
        Path examplesDir = Path.of("src/test/resources/examples");
        assertTrue(Files.exists(examplesDir), "There is no folder with code examples: " + examplesDir);

        try (Stream<Path> paths = Files.list(examplesDir)) {
            List<Path> files = paths
                    .filter(p -> p.toString().endsWith(".txt"))
                    .sorted()
                    .collect(Collectors.toList());

            assertFalse(files.isEmpty(), "There are no test files in " + examplesDir);

            System.out.println("Found " + files.size() + " test files in " + examplesDir);

            int passed = 0;
            for (Path file : files) {
                runTestFile(file);
                passed++;
                System.out.println("✔ Passed: " + file.getFileName());
            }

            System.out.println("✅ All " + passed + " tests passed successfully.");
        }
    }

    private void runTestFile(Path file) throws IOException {
        String content = Files.readString(file);
        String[] parts = content.split("(?m)^---$");
        assertEquals(3, parts.length, "File must contain 3 sections: " + file);

        String code = parts[0].trim();
        String expectedTokens = parts[1].trim();
        String expectedOutput = parts[2].trim();

        // tokenization
        List<Token> tokens = lexer.tokenize(code);

        // Collect tokens as strings
        String actualTokens = tokens.stream()
                .map(Token::toString)
                .collect(Collectors.joining("\n"));

        // Assert tokens
        assertEquals(expectedTokens, actualTokens,
                "File: " + file.getFileName() + " - tokens did not match");
    }
}
