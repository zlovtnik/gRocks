# Well-Instructed Copilot Instructions for Gleam Project Development

These are guidelines for assisting with the Gleam data processing pipeline project using functional programming and monads.

## Language Focus
All code must be in Gleam syntax. Use static types everywhere (e.g., fn add(a: Int, b: Int) -> Int). Prioritize functional programming: immutability, pure functions, recursion/folds over loops, pattern matching, and algebraic data types (enums).

## Monads Integration
When handling errors or optional values, use Gleam's Result or Option with the use expression for monadic chaining. Provide a bind function like attempt if needed. Explain how it relates to monads (e.g., "This flattens nested Results like monadic bind in Haskell").

## Query Handling
- If asked for code snippets: Generate complete, runnable examples with imports (e.g., import gleam/io). Include type signatures and comments explaining FP concepts.
- If asked for explanations: Break down concepts simply (e.g., "Immutability means variables can't change, forcing new values for updates—prevents side effects").
- If reporting errors: Suggest fixes with diffs or rewritten code. Check for common issues like non-exhaustive matches.
- If wanting extensions: Suggest FP-friendly additions, like higher-order functions or generics, without mutability.

## Best Practices
Encourage small functions, exhaustive patterns, and tests. Avoid side effects except in main. Use gleam_stdlib modules like list, result, io.

## Response Style
Be concise but thorough. Use bullet points for steps, code blocks for Gleam code. If unclear, ask clarifying questions (e.g., "What specific error are you seeing?").

## Limits
Do not suggest non-FP patterns (e.g., no mutable vars). Stick to Gleam ecosystem—no external langs unless requested.