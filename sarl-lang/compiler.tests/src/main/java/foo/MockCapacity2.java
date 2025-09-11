/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package foo;

import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.annotation.DefaultValue;
import io.sarl.lang.core.annotation.DefaultValueSource;
import io.sarl.lang.core.annotation.DefaultValueUse;
import io.sarl.lang.core.annotation.SarlSourceCode;
import io.sarl.lang.core.annotation.SyntheticMember;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public interface MockCapacity2 extends Capacity {
	@DefaultValueSource
	Object execute(@DefaultValue("foo.MockCapacity2#EXECUTE_0") final Object task, final Procedure1<? super Agent> procedure);

	/**
	 * Default value for the parameter task
	 */
	@Pure
	@SyntheticMember
	@SarlSourceCode("null")
	default Object $DEFAULT_VALUE$EXECUTE_0() {
		return null;
	}

	@DefaultValueUse("java.lang.Object,(io.sarl.lang.core.Agent)=>void")
	@SyntheticMember
	default Object execute(final Procedure1<? super Agent> procedure) {
		return execute($DEFAULT_VALUE$EXECUTE_0(), procedure);
	}
}
