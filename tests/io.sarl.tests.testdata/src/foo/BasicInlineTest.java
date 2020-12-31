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


package foo;

import org.eclipse.xtext.xbase.lib.Inline;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;

/** This function provides inlined functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings({"static-method", "javadoc", "unused"})
public class BasicInlineTest {

	public boolean isMe(Address adr) {
		return false;
	}
	
	@Inline(value = "($1 != null && $0isMe($1.getSource()))", constantExpression = true)
	public boolean isFromMe( Event event) {
		return false;
	}
	
	@Inline(value = "($1.getID() != null)", constantExpression = true)
	public boolean isMe2(Address adr) {
		return false;
	}
	
	@Inline(value = "($1 != null && $0isMe2($1.getSource()))", constantExpression = true)
	public boolean isFromMe2(Event event) {
		return false;
	}

	@Inline(value = "($1.getID() != null)", constantExpression = true)
	public boolean isMe3(Address adr) {
		return false;
	}
	
	@Inline(value = "($1 != null && $1.getSource().getID() != null)", constantExpression = true)
	public boolean isFromMe3(Event event) {
		return false;
	}

}
