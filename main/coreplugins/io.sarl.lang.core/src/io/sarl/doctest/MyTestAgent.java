/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.doctest;

import java.util.UUID;

import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.core.Agent;

/** This is an agent.
 */
@SuppressWarnings("all")
@SarlElementType(19)
public final class MyTestAgent extends Agent {

	public MyTestAgent(UUID parentID, UUID agentID) {
		super(parentID, agentID);
	}

	public void my$Fct(int a) {}
	
	public void myFct(int... a) {}

}
