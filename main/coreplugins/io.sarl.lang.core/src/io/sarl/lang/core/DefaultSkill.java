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

package io.sarl.lang.core;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Annotation for associating a capacity and a default skill.
 *
 * <p>This annotation should be associated to a {@code Capacity}.
 * It takes a value that is the type of the skill that should be given to an
 * agent by default when it tries to use the capacity.
 * This annotation is similar to the {@code @ImplementedBy} annotation into
 * the injection library.
 *
 * <p>The SRE could use this annotation to create dynamically the skill when
 * the {@code Agent.getSkill()} is called.
 *
 * <p>This annotation is supported by an implementation of {@link DynamicSkillProvider}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see DynamicSkillProvider
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface DefaultSkill {

	/** The type of the implementation skill.
	 *
	 * @return the type of the implementation skill.
	 */
	Class<? extends Skill> value();

}
