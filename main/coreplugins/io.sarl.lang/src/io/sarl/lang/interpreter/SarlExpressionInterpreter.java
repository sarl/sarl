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

package io.sarl.lang.interpreter;

import java.util.function.UnaryOperator;
import javax.inject.Inject;

import com.google.inject.Provider;
import org.eclipse.xtext.common.types.util.JavaReflectAccess;
import org.eclipse.xtext.xbase.interpreter.IEvaluationContext;
import org.eclipse.xtext.xbase.interpreter.impl.XbaseInterpreter;

/** Interpreter of SARL expressions (not SARL declaration types).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class SarlExpressionInterpreter extends XbaseInterpreter {

	private ClassLoader classLoader;

	/** Injection constructor.
	 */
	public SarlExpressionInterpreter() {
		//
	}

	/** Constructor.
	 *
	 * @param contextProvider the provider of evaluation context.
	 * @param javaReflectAccess the accessor to the Java types.
	 * @param loader the class loader to be used.
	 */
	public SarlExpressionInterpreter(Provider<IEvaluationContext> contextProvider, JavaReflectAccess javaReflectAccess,
			ClassLoader loader) {
		super(contextProvider, javaReflectAccess, loader);
	}

	@Override
	@Inject
	public void setClassLoader(ClassLoader classLoader) {
		super.setClassLoader(classLoader);
		this.classLoader = classLoader;
	}

	/** Expand the current class loader with the class loader that is build with the given operator.
	 * The interpreter's class loader is the result of the given builder to which the current class loader is provided.
	 *
	 * @param builder the class loader builder.
	 * @return the expand class loader.
	 */
	public ClassLoader expandClassLoader(UnaryOperator<ClassLoader> builder) {
		final ClassLoader expandClassLoader =  builder.apply(this.classLoader);
		super.setClassLoader(expandClassLoader);
		return expandClassLoader;
	}

}
