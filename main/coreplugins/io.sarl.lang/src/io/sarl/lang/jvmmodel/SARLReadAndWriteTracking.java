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

package io.sarl.lang.jvmmodel;

import javax.inject.Singleton;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.xbase.validation.ReadAndWriteTracking;

/** Track the initialization, read and write accesses to a field.
 *
 * <p>This implementation extends the Xbase one with the write accesses.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
@Singleton
public class SARLReadAndWriteTracking extends ReadAndWriteTracking {

	private static final Adapter ASSIGNMENT_MARKER = new Adapter() {
		@Override
		public Notifier getTarget() {
			return null;
		}

		@Override
		public boolean isAdapterForType(final Object type) {
			return false;
		}

		@Override
		public void notifyChanged(final Notification notification) {
			//
		}

		@Override
		public void setTarget(final Notifier newTarget) {
			//
		}
	};

	/** Mark the given object as an assigned object after its initialization.
	 *
	 * <p>The given object has its value changed by a assignment operation.
	 *
	 * @param object the written object.
	 * @return {@code true} if the write flag has changed.
	 */
	public boolean markAssignmentAccess(EObject object) {
		assert object != null;
		if (!isAssigned(object)) {
			return object.eAdapters().add(ASSIGNMENT_MARKER);
		}
		return false;
	}

	/** Replies if the given object was marked as assigned within the current compilation unit.
	 *
	 * @param object the object to test.
	 * @return {@code true} if the object was written within the current compilation unit;
	 *    {@code false} if is is not written within the current compilation unit.
	 */
	@SuppressWarnings("static-method")
	public boolean isAssigned(final EObject object) {
		assert object != null;
		return object.eAdapters().contains(ASSIGNMENT_MARKER);
	}

}
