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

package io.sarl.lang.compiler.batch;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.xtext.util.CancelIndicator;

/** A wrapper from a CancelIndicator to a ProgressMonitor.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class CancelIndicatorProgressMonitor implements IProgressMonitor {

	private final CancelIndicator delegate;

	private boolean canceled;

	/** Constructor.
	 *
	 * @param delegate the cancel indicator to wrap.
	 */
	public CancelIndicatorProgressMonitor(final CancelIndicator delegate) {
		super();
		assert delegate != null;
		this.delegate = delegate;
	}

	@Override
	public boolean isCanceled() {
		return this.canceled || this.delegate.isCanceled();
	}

	@Override
	public void setCanceled(final boolean value) {
		this.canceled = value;
	}

	@Override
	public void beginTask(final String name, final int totalWork) {
		//
	}

	@Override
	public void setTaskName(final String name) {
		//
	}

	@Override
	public void subTask(final String name) {
		//
	}

	@Override
	public void internalWorked(final double work) {
		//
	}

	@Override
	public void worked(final int work) {
		//
	}

	@Override
	public void done() {
		//
	}

}
