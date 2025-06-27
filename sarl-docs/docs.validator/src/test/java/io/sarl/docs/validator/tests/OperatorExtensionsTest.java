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

package io.sarl.docs.validator.tests;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import io.sarl.docs.validator.OperatorExtensions;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@SuppressWarnings("all")
@DisplayName("OperatorExtensions")
@org.junit.jupiter.api.Tag("maven")
public class OperatorExtensionsTest {

	private List<List<String>> input;

	@BeforeEach
	public void setUp() {
		this.input = new ArrayList<>();
		this.input.add(Arrays.asList("$v=$i"));
		this.input.add(Arrays.asList("$i||$i"));
		this.input.add(Arrays.asList("$i&&$i"));
		this.input.add(Arrays.asList("$i==$i", "$i!=$i", "$i===$i", "$i!==$i"));
		this.input.add(Arrays.asList("$i>=$i", "$i<=$i", "$i<$i", "$i>$i"));
		this.input.add(Arrays.asList("$o instanceof $t"));
		this.input.add(Arrays.asList("$i<=>$i", "$i<>$i", "$i..$i", "$i>..$i", "$i..<$i", "$i->$i", "$i=>$i", "$i?:$i", "$i>>$i", "$i<<$i", "$i>>>$i", "$i<<<$i"));
		this.input.add(Arrays.asList("$i+$i", "$i-$i"));
		this.input.add(Arrays.asList("$i*$i", "$i/$i", "$i%$i"));
		this.input.add(Arrays.asList("$L as $t"));
		this.input.add(Arrays.asList("$i**$i"));
		this.input.add(Arrays.asList("!$R", "-$R", "+$R"));
		this.input.add(Arrays.asList("$v++", "$v--"));
	}
	
	@Test
	@DisplayName("validateOperatorOrder")
	public void validateOperatorOrder() throws Exception {
		final var actual = OperatorExtensions.validateOperatorOrder(this.input, null);
		assertEquals(0, actual);
	}

}
