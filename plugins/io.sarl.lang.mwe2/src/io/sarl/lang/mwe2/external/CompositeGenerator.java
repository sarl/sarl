/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.mwe2.external;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.eclipse.xtext.Grammar;

/**
 * A generator composed of external language specification generators.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class CompositeGenerator extends ExternalLanguageSpecificationGenerator
		implements ExternalLanguageSpecificationContext {

	private final List<ExternalLanguageSpecificationGenerator> generators = new ArrayList<>();

	@Override
	public void addGenerator(ExternalLanguageSpecificationGenerator generator) {
		if (generator != null) {
			this.generators.add(generator);
			generator.setContext(this);
		}
	}

	@Override
	protected String getHumanReadableSpecificationName() {
		return null;
	}

	@Override
	public void generate(Grammar grammar) {
		for (ExternalLanguageSpecificationGenerator generator : this.generators) {
			generator.generate(grammar);
		}
	}

	@Override
	protected final void generate(Set<String> literals, Set<String> keywords, Set<String> punctuation, Set<String> ignored) {
		throw new UnsupportedOperationException();
	}

}

