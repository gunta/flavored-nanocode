# nanocode - Synth

A nanocode implementation in [Synth](https://github.com/code-agents/synth), an intent-based programming language.

## What is Synth?

Synth is a visionary/proposed programming paradigm that separates **intent** (what you want) from **implementation** (how to execute it). It's designed for human-AI collaboration where:

- **Intent files** (`.intent.synth`) describe outcomes, flows, and capabilities in human-readable terms
- **Implementation files** (`.impl.synth`) provide execution hints, optimizations, and runtime details

## Files

- `nanocode.intent.synth` - Describes the agent's purpose, capabilities, and behavior
- `nanocode.impl.synth` - Defines execution strategies and optimizations

## Conceptual Execution

```bash
# Synth is conceptual - this is how it might work:
synth run nanocode.intent.synth
```

## Philosophy

Traditional programming:
```javascript
// HOW to do something
const result = [];
for (const item of items) {
  if (item.score > threshold) {
    result.push(transform(item));
  }
}
return result.sort((a, b) => b.score - a.score).slice(0, 10);
```

Synth intent-based:
```synth
// WHAT you want
intent filterAndRank {
  input: items
  output: topItems
  
  flow = { filter, transform, rank }
  
  optimize {
    target: relevance
    limit: 10
  }
}
```

The AI compiler handles the implementation details.
