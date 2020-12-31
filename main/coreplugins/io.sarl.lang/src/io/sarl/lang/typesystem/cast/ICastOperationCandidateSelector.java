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

package io.sarl.lang.typesystem.cast;

import com.google.inject.ImplementedBy;
import org.eclipse.xtext.xbase.scoping.batch.IIdentifiableElementDescription;
import org.eclipse.xtext.xbase.typesystem.internal.AbstractTypeComputationState;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/** Select the functions that should be called in place of a classic cast operator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
@FunctionalInterface
@ImplementedBy(ObjectAndPrimitiveBasedCastOperationCandidateSelector.class)
public interface ICastOperationCandidateSelector {

	/** Replies if the given description is a valid candidate for a cast linked operation.
	 *
	 * @param state the current state of the type computation.
	 * @param castType the target type.
	 * @param expressionType the type of the expression to cast.
	 * @return the validator, never {@code null}.
	 */
	ISelector prepare(
			AbstractTypeComputationState state, LightweightTypeReference castType,
			LightweightTypeReference expressionType);

	/** Do the selection of a specific operation description.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	@FunctionalInterface
	public interface ISelector {

		/** Replies if the given description is a valid candidate for a cast linked operation.
		 *
		 * @param description the description of the found operation.
		 * @return {@code true} if the operation is a valid cast linked operation; otherwise {@code false}.
		 */
		boolean isCastOperatorCandidate(IIdentifiableElementDescription description);

	}

}
