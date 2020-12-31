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

package io.sarl.lang.typesystem;

import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.computation.IFeatureLinkingCandidate;

/**
 * Adapter to memorize the type computing candidates for an abstract feature call.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class FeatureCallAdapter extends AdapterImpl {

	private List<? extends IFeatureLinkingCandidate> candidates;

	@Override
	public boolean isAdapterForType(Object type) {
		return FeatureCallAdapter.class.equals(type);
	}

	/** Change the call candidates.
	 *
	 * @param candidates the candidates.
	 */
	public void setCallCandidates(List<? extends IFeatureLinkingCandidate> candidates) {
		this.candidates = candidates;
	}

	/** Replies the call candidates.
	 *
	 * @return the candidates.
	 */
	@Pure
	public List<? extends IFeatureLinkingCandidate> getCallCandidates() {
		if (this.candidates == null) {
			this.candidates = Collections.emptyList();
		}
		return this.candidates;
	}

}
