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

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.core.annotation.DefaultValue;
import io.sarl.lang.core.annotation.DefaultValueSource;
import io.sarl.lang.core.annotation.DefaultValueUse;
import io.sarl.lang.core.annotation.SarlSourceCode;
import io.sarl.lang.core.annotation.SyntheticMember;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 */
@SuppressWarnings("all")
public abstract class Type2 {
	@DefaultValueSource
	public abstract void fct0(@DefaultValue("foo.Type2#FCT0_0") final String param0, final int param1);

	@Pure
	@SyntheticMember
	@SarlSourceCode("\"abc\"")
	private final String $DEFAULT_VALUE$FCT0_0() {
		return "abc";
	}

	@DefaultValueUse("java.lang.String,int")
	@SyntheticMember
	public final void fct0(final int param1) {
		fct0($DEFAULT_VALUE$FCT0_0(), param1);
	}
}
