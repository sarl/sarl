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

import com.google.inject.ImplementedBy;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.xbase.XExpression;

/** Detector of the type of access to the default value of a formal parameter.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@ImplementedBy(DefaultValueAccessDetector.class)
public interface IDefaultValueAccessDetector {

	/** Replies if the given default value should be stored into a static field.
	 *
	 * @param sourceObject the source of the generation.
	 * @param defaultValue the default value to test.
	 * @param parameterContainer the container of the formal parameter.
	 * @param executableContainer the container of the executable.
	 * @return {@code true} if the default value should be stored into a static field.
	 */
	boolean isStaticFieldStorage(EObject sourceObject, XExpression defaultValue, JvmExecutable parameterContainer, JvmGenericType executableContainer);

	/** Replies if the given operation is a default-value operation that contains only references to static features.
	 *
	 * @param operation the operation to test.
	 * @return {@code true} if the default value should be accessed statically.
	 */
	boolean isStaticAccess(JvmOperation operation);

}
