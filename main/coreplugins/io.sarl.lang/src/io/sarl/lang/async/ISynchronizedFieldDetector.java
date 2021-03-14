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

package io.sarl.lang.async;

import com.google.inject.ImplementedBy;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Helper that determine if a field is synchronized or not.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@ImplementedBy(StandardSynchronizedFieldDetector.class)
public interface ISynchronizedFieldDetector {

	/** Check if the given field is considered as synchronized.
	 *
	 * <p>A field is synchronized when it has a thread-safe type, or declared with the volatile or final modifiers (in the case
	 * of primitive types).
	 *
	 * @param field the field to test
	 * @return {@code true} if the given field is synchronized, otherwise {@code false}.
	 */
	@Pure
	boolean isSynchronizedField(XtendField field);

}
