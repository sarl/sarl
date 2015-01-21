/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.bugfixes;

import org.eclipse.xtext.Action;
import org.eclipse.xtext.ParserRule;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.serializer.analysis.ContextPDAProvider;
import org.eclipse.xtext.serializer.analysis.ISerState;
import org.eclipse.xtext.serializer.analysis.SerializerPDA.SerializerPDACloneFactory;
import org.eclipse.xtext.serializer.analysis.SerializerPDA.SerializerPDAElementFactory;
import org.eclipse.xtext.serializer.analysis.SerializerPDA.SerializerPDAGetToken;
import org.eclipse.xtext.util.formallang.FollowerFunctionImpl;
import org.eclipse.xtext.util.formallang.Pda;

import com.google.inject.Singleton;

/** This class is used by the serializer for fixing the
 * <a href="https://github.com/sarl/sarl/issues/277">issue #277</a>.
 *
 * This issue is due to an invalid behavior of the Xtext serializer
 * against the interpretation of <code>('a' & 'b')</code>.
 *
 * This class provides a temporary workaround.
 *
 * FIXME: Remove this class when the Xtext serializer is fixed.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class SARLContextPDAProvider extends ContextPDAProvider {

	/**
	 */
	public SARLContextPDAProvider() {
		//
	}

	@Override
	protected Pda<ISerState, RuleCall> createPDA(ParserRule rule) {
		// This part of the code is copied from the overridded createPDA function.
		SerializerParserRuleCfg cfg = new SerializerParserRuleCfg(rule);
		SerializerParserRuleFollowerFunction ff = new SerializerParserRuleFollowerFunction(cfg, rule);


		// Issue #277: treat unordered groups as regular groups
		ff.setUnorderedStrategy(FollowerFunctionImpl.UnorderedStrategy.SEQUENCE);


		// This part of the code is copied from the overridded createPDA function.
		Pda<ISerState, RuleCall> pda = this.pdaUtil.create(cfg, ff, new SerializerPDAElementFactory());
		return this.pdaUtil.filterOrphans(pda, new SerializerPDACloneFactory());
	}

	@Override
	protected Pda<ISerState, RuleCall> createPDA(Action action) {
		// This part of the code is copied from the overridded createPDA function.
		SerializerActionCfg cfg = new SerializerActionCfg(action);
		SerializerActionFollowerFunction ff = new SerializerActionFollowerFunction(cfg, action);


		// Issue #277: treat unordered groups as regular groups
		ff.setUnorderedStrategy(FollowerFunctionImpl.UnorderedStrategy.SEQUENCE);


		// This part of the code is copied from the overridded createPDA function.
		SerializerPDAElementFactory fact = new SerializerPDAElementFactory();
		Pda<ISerState, RuleCall> actionpda = this.pdaUtil.create(cfg, ff, fact);
		SerializerPDAGetToken getToken = new SerializerPDAGetToken();
		Pda<ISerState, RuleCall> expandedpda = this.pdaUtil.expand(actionpda, new ExpandRuleCalls() {
			//
		}, getToken, fact);
		Pda<ISerState, RuleCall> filteredpda = this.pdaUtil.filterOrphans(expandedpda, new SerializerPDACloneFactory());
		return filteredpda;
	}

}
