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

package io.sarl.lang.scoping;

import com.google.common.base.Strings;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.xbase.XbaseQualifiedNameConverter;

import io.sarl.lang.util.SarlUtils;

/** This class is fixing the
 * <a href="https://github.com/sarl/sarl/issues/356">issue #356</a>.
 *
 * <p>This issue is due to a JvmDeclaredType with a null name.
 *
 * <p>This class provides a temporary workaround.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLQualifiedNameConverter extends XbaseQualifiedNameConverter {

	@Override
	public QualifiedName toQualifiedName(String qualifiedNameAsString) {
		if (Strings.isNullOrEmpty(qualifiedNameAsString)) {
			return QualifiedName.create("io.sarl.lostAndFound", SarlUtils.HIDDEN_MEMBER_CHARACTER //$NON-NLS-1$
					+ SarlUtils.HIDDEN_MEMBER_CHARACTER + "Foo"); //$NON-NLS-1$
		}
		return super.toQualifiedName(qualifiedNameAsString);
	}

}
