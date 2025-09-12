# Agent-Oriented Programming in SARL

Agent-Oriented Programming (AOP) is a paradigm that focuses on the development of software systems composed of autonomous, interacting agents. The SARL programming language is designed to facilitate the creation of such systems by providing a robust and expressive syntax for defining agents, their behaviors, interactions, and environments.

This collection of documentation pages serves as a foundational reference for developers and researchers working with SARL. Each document delves into a specific aspect of agent-oriented programming, offering detailed explanations and practical insights into the language's features and capabilities.

## Core Concepts and Features

The core concepts of SARL are defined in the [SARL metamodel](./Metamodel.md). It provides fundamental abstractions for implementing the individual, social and collective dimensions of a multi-agent system.

### Agents

Agents are the fundamental building blocks of SARL applications. They encapsulate state, behavior, and communication capabilities, enabling them to act autonomously and interact with other agents or their environment. The [Agent Reference](./Agent.md) document provides a thorough exploration of agent definition, lifecycle, and the mechanisms for agent interaction and coordination.

### Behaviors

Behaviors define how agents respond to stimuli, such as events or messages. In SARL, behaviors are modular and can be dynamically added or removed, allowing agents to adapt their actions based on context. The [Behavior Reference](./Behavior.md) document outlines the syntax and semantics of behavior definition, including event handling and action execution.

### Capacities and Skills

Capacities and skills extend the functionality of agents by providing reusable and composable units of behavior. Capacities represent high-level abilities, while skills offer specific implementation of capacities. The [Capacity Reference](./Capacity.md) and [Skill Reference](./Skill.md) documents detail how to define, implement, and utilize these constructs to enhance agent capabilities.

#### Built-in Capacities

SARL includes a set of built-in capacities that provide essential functionalities, such as logging, lifecycle management, and interaction with the environment. The [Built-in Capacity Reference](./BIC.md) document offers an overview of these capacities, their usage, and how they can be integrated into agent designs.

### Events

Events are the primary mechanism for communication among agents. They can represent external stimuli, internal state changes, or messages from other agents. The [Event Reference](./Event.md) document describes the syntax for defining events, their propagation, and how agents can react to them.

### Spaces

Spaces in SARL represent the environments in which agents can communicate. They provide a context for agent interactions, resource sharing, and coordination. The [Space Reference](./Space.md) document explores the definition and management of spaces.

### Failure Management

Robustness is a critical aspect of agent-oriented systems, particularly in handling failures and validation errors. The [Management of the Failures and Validation Errors](./Failures.md) document discusses strategies for fault tolerance, error recovery, and the maintenance of system integrity in the face of unexpected events.


[:Include:](../../includes/legal.inc)
