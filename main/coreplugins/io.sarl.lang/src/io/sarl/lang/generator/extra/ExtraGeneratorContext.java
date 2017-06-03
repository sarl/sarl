/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.generator.extra;

import org.eclipse.xtext.generator.IGenerator2;
import org.eclipse.xtext.generator.IGeneratorContext;
import org.eclipse.xtext.util.CancelIndicator;

/** Context for the extra generator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ExtraGeneratorContext implements IGeneratorContext {

	private final IGeneratorContext delegate;

	private Iterable<IGenerator2> extraGenerators;

	/** Create the context for the given delegate.
	 *
	 * @param delegate the delegate.
	 * @param extraGenerators the extra generators that could be used in this context.
	 */
	public ExtraGeneratorContext(IGeneratorContext delegate, Iterable<IGenerator2> extraGenerators) {
		this.delegate = delegate;
		this.extraGenerators = extraGenerators;
	}

	@Override
	public CancelIndicator getCancelIndicator() {
		return this.delegate.getCancelIndicator();
	}

	/** Replies the delegate.
	 *
	 * @return the delegate.
	 */
	public IGeneratorContext getDelegate() {
		return this.delegate;
	}

	/** Replies the extra generators.
	 *
	 * @return the extra generators.
	 */
	public Iterable<IGenerator2> getExtraGenerators() {
		return this.extraGenerators;
	}

}
