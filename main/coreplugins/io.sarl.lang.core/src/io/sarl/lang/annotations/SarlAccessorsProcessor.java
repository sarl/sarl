/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.annotations;

import javax.inject.Singleton;

import org.eclipse.xtend.lib.annotations.AccessorsProcessor;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.Visibility;

import io.sarl.lang.core.Agent;

/** Processor for the {@code @Accessors} active annotations.
 *
 * <p>This processor ensures that the visibility of the generated functions is not higher
 * than the visility allowed into the containing type.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
@Singleton
public class SarlAccessorsProcessor extends AccessorsProcessor {

	@Override
	protected void _transform(MutableFieldDeclaration it, TransformationContext context) {
	    final AccessorsProcessor.Util util = new AccessorsProcessor.Util(context);
	    if (util.shouldAddGetter(it)) {
	    	Visibility visibility = util.toVisibility(util.getGetterType(it));
	    	visibility = applyMinMaxVisibility(visibility, it, context);
	    	util.addGetter(it, visibility);
	    }
	    if (util.shouldAddSetter(it)) {
	    	Visibility visibility = util.toVisibility(util.getSetterType(it));
	    	visibility = applyMinMaxVisibility(visibility, it, context);
	    	util.addSetter(it, visibility);
	    }
	}

	/** Apply the minimum and maximum visibilities to the given one.
	 *
	 * @param visibility the visibility.
	 * @param it the field associated to the accessors to generate.
	 * @param context the transformation context.
	 * @return the given {@code visibility}, or the min/max visibility if the given one is too high.
	 */
	@SuppressWarnings("static-method")
	protected Visibility applyMinMaxVisibility(Visibility visibility, MutableFieldDeclaration it, TransformationContext context) {
		if (context.findTypeGlobally(Agent.class).isAssignableFrom(it.getDeclaringType())) {
			if (visibility.compareTo(Visibility.PROTECTED) > 0) {
				return Visibility.PROTECTED;
			}
		}
		return visibility;
	}

}
