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

package io.sarl.lang.sarl.actionprototype;

import java.lang.ref.WeakReference;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/**
 * This class permits to wrap the formal parameters.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class InferredStandardParameter {

	/** Original parameter.
	 */
	protected final WeakReference<EObject> source;

	/** Name.
	 */
	protected final String name;

	/** Name.
	 */
	protected final LightweightTypeReference type;

	/** Default value annotation value.
	 */
	protected String defaultValueAnnotationValue;

	/** Basename of default value annotation value.
	 */
	protected String defaultValueAnnotationValueBasename;

	/** The value of the calling argument.
	 */
	protected DynamicArgumentName dynamicArgument;

	/** Constructor.
	 * @param source the original parameter.
	 * @param name the name of the formal parameter.
	 * @param type the type of the formal parameter.
	 * @param dynamicArgument the argument name that could be changed dynamically.
	 * @since 0.12
	 */
	public InferredStandardParameter(EObject source, String name, LightweightTypeReference type, DynamicArgumentName dynamicArgument) {
		this.source = new WeakReference<>(source);
		this.name = name;
		this.type = type;
		this.dynamicArgument = dynamicArgument;
	}

	/** Replies the value of the calling argument.
	 *
	 * @return the value of the calling argument.
	 * @since 0.12
	 */
	public DynamicArgumentName getDynamicCallingArgument() {
		return this.dynamicArgument;
	}

	/** Replies the source parameter.
	 *
	 * @return the source parameter.
	 */
	public EObject getParameter() {
		return this.source.get();
	}

	/** Replies the name.
	 *
	 * @return the name.
	 */
	public String getName() {
		return this.name;
	}

	/** Replies the type of the parameter.
	 *
	 * @return the type.
	 */
	public LightweightTypeReference getType() {
		return this.type;
	}

	@Override
	public String toString() {
		return this.name + " : " + this.type; //$NON-NLS-1$
	}

	/** Replies the value of the annotation that is marked this parameter with a default value.
	 *
	 * @return the annotation's value.
	 */
	public String getDefaultValueAnnotationValue() {
		return this.defaultValueAnnotationValue;
	}

	/** Replies the basename of the value of the annotation that is marked this parameter with a default value.
	 *
	 * @return the basename of the annotation's value.
	 */
	public String getDefaultValueAnnotationValueBasename() {
		return this.defaultValueAnnotationValueBasename;
	}

	/** Set the value of the annotation that is marked this parameter with a default value.
	 *
	 * @param value the annotation's value.
	 * @param basename the basename of the value.
	 */
	void setDefaultValueAnnotationValue(String value, String basename) {
		this.defaultValueAnnotationValue = value;
		this.defaultValueAnnotationValueBasename = basename;
	}

}
