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
package io.sarl.eclipse.wizards.elements;

import io.sarl.lang.SARLKeywords;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;

import java.util.Collection;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jdt.core.IPackageFragment;

import com.google.common.base.Strings;


/** Utilities for creating SARL elements.
 *
 * This class may extend the XtendTypeCreatorUtil.
 *
 * FIXME: Creating the code from the EMF elements (using a serializer) should be better than this hard-coded version.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SarlTypeCreatorUtil {

	private SarlTypeCreatorUtil() {
		//
	}

	/** Create a package declaration.
	 *
	 * @param packageFragment - package fragment.
	 * @param lineSeparator - line separator.
	 * @return the SARL package declaration.
	 */
	public static String createPackageDeclaration(IPackageFragment packageFragment, String lineSeparator) {
		StringBuilder sb = new StringBuilder();
		if (!Strings.isNullOrEmpty(packageFragment.getElementName())) {
			sb.append(SARLKeywords.PACKAGE + " "); //$NON-NLS-1$
			sb.append(packageFragment.getElementName());
			sb.append(lineSeparator);
		}
		return sb.toString();
	}

	/** Create the SARL code that corresponds to an agent with the given properties.
	 *
	 * @param packageName - the name of the package where the agent is created.
	 * @param agentName - name of the agent type.
	 * @param superClass - the name of the super type.
	 * @param imports - the list of the types to put into the import section for supporting
	 *                  the generated code. If this parameter is <code>null</code>, the
	 *                  list is not filled.
	 * @param indentation - identation string.
	 * @param lineSeparator - line separator.
	 * @param generateInitializeHandler - indicates if the handler for the Initialize event should be generated.
	 * @return the SARL code for the agent.
	 */
	public static String createAgentContent(
			String packageName,
			String agentName, String superClass,
			Set<String> imports,
			String indentation, String lineSeparator,
			boolean generateInitializeHandler) {
		StringBuilder sb = new StringBuilder();
		sb.append(SARLKeywords.AGENT);
		sb.append(' ');
		sb.append(agentName);

		if (!Strings.isNullOrEmpty(superClass) && !Agent.class.getName().equals(superClass)) {
			sb.append(' ');
			sb.append(SARLKeywords.EXTENDS);
			sb.append(' ');
			String superClassname = stripPackage(superClass);
			if (agentName.equals(superClassname)) {
				sb.append(superClass);
			} else {
				sb.append(superClassname);
				addImport(packageName, imports, superClass);
			}
		}

		sb.append(" {"); //$NON-NLS-1$
		sb.append(lineSeparator);
		sb.append(indentation);
		if (generateInitializeHandler) {
			sb.append(SARLKeywords.ON);
			String initializeEvent = "io.sarl.core.Initialize"; //$NON-NLS-1$
			sb.append(' ');
			sb.append(stripPackage(initializeEvent));
			sb.append(" {"); //$NON-NLS-1$
			addImport(packageName, imports, initializeEvent);
			sb.append(lineSeparator);
			sb.append(indentation);
			sb.append(indentation);
			sb.append(lineSeparator);
			sb.append(indentation);
			sb.append("}"); //$NON-NLS-1$
		}
		sb.append(lineSeparator);
		sb.append("}"); //$NON-NLS-1$
		sb.append(lineSeparator);
		return sb.toString();
	}

	/** Create the SARL code that corresponds to a behavior with the given properties.
	 *
	 * @param packageName - the name of the package where the behavior is created.
	 * @param behaviorName - name of the behavior type.
	 * @param superClass - the name of the super type.
	 * @param imports - the list of the types to put into the import section for supporting
	 *                  the generated code. If this parameter is <code>null</code>, the
	 *                  list is not filled.
	 * @param indentation - identation string.
	 * @param lineSeparator - line separator.
	 * @return the SARL code for the behavior.
	 */
	public static String createBehaviorContent(
			String packageName,
			String behaviorName, String superClass,
			Set<String> imports,
			String indentation, String lineSeparator) {
		StringBuilder sb = new StringBuilder();
		sb.append(SARLKeywords.BEHAVIOR);
		sb.append(' ');
		sb.append(behaviorName);

		if (!Strings.isNullOrEmpty(superClass) && !Behavior.class.getName().equals(superClass)) {
			sb.append(' ');
			sb.append(SARLKeywords.EXTENDS);
			sb.append(' ');
			String superClassname = stripPackage(superClass);
			if (behaviorName.equals(superClassname)) {
				sb.append(superClass);
			} else {
				sb.append(superClassname);
				addImport(packageName, imports, superClass);
			}
		}

		sb.append(" {"); //$NON-NLS-1$
		sb.append(lineSeparator);

		sb.append(indentation);
		sb.append(SARLKeywords.CONSTRUCTOR);
		sb.append("(owner : "); //$NON-NLS-1$
		String agentType = "io.sarl.lang.core.Agent"; //$NON-NLS-1$
		String agentBasename = stripPackage(agentType);
		if (agentBasename.equals(behaviorName)) {
			sb.append(agentType);
		} else {
			sb.append(agentBasename);
			addImport(packageName, imports, agentType);
		}
		sb.append(") {"); //$NON-NLS-1$
		sb.append(lineSeparator);
		sb.append(indentation);
		sb.append(indentation);
		sb.append("super(owner)"); //$NON-NLS-1$
		sb.append(lineSeparator);
		sb.append(indentation);
		sb.append("}"); //$NON-NLS-1$
		sb.append(lineSeparator);

		sb.append("}"); //$NON-NLS-1$
		sb.append(lineSeparator);
		return sb.toString();
	}

	/** Create the SARL code that corresponds to a capacity with the given properties.
	 *
	 * @param packageName - the name of the package where the capacity is created.
	 * @param capacityName - name of the capacity type.
	 * @param superClass - the name of the super type.
	 * @param imports - the list of the types to put into the import section for supporting
	 *                  the generated code. If this parameter is <code>null</code>, the
	 *                  list is not filled.
	 * @param indentation - identation string.
	 * @param lineSeparator - line separator.
	 * @return the SARL code for the capacity.
	 */
	public static String createCapacityContent(
			String packageName,
			String capacityName, String superClass,
			Set<String> imports,
			String indentation, String lineSeparator) {
		StringBuilder sb = new StringBuilder();
		sb.append(SARLKeywords.CAPACITY);
		sb.append(' ');
		sb.append(capacityName);

		if (!Strings.isNullOrEmpty(superClass) && !Capacity.class.getName().equals(superClass)) {
			sb.append(' ');
			sb.append(SARLKeywords.EXTENDS);
			sb.append(' ');
			String superClassname = stripPackage(superClass);
			if (capacityName.equals(superClassname)) {
				sb.append(superClass);
			} else {
				sb.append(superClassname);
				addImport(packageName, imports, superClass);
			}
		}

		sb.append(" {"); //$NON-NLS-1$
		sb.append(lineSeparator);

		sb.append(indentation);
		sb.append(lineSeparator);

		sb.append("}"); //$NON-NLS-1$
		sb.append(lineSeparator);
		return sb.toString();
	}

	/** Create the SARL code that corresponds to an event with the given properties.
	 *
	 * @param packageName - the name of the package where the event is created.
	 * @param eventName - name of the event type.
	 * @param superClass - the name of the super type.
	 * @param imports - the list of the types to put into the import section for supporting
	 *                  the generated code. If this parameter is <code>null</code>, the
	 *                  list is not filled.
	 * @param indentation - identation string.
	 * @param lineSeparator - line separator.
	 * @return the SARL code for the event.
	 */
	public static String createEventContent(
			String packageName,
			String eventName, String superClass,
			Set<String> imports,
			String indentation, String lineSeparator) {
		StringBuilder sb = new StringBuilder();
		sb.append(SARLKeywords.EVENT);
		sb.append(' ');
		sb.append(eventName);

		if (!Strings.isNullOrEmpty(superClass) && !Event.class.getName().equals(superClass)) {
			sb.append(' ');
			sb.append(SARLKeywords.EXTENDS);
			sb.append(' ');
			String superClassname = stripPackage(superClass);
			if (eventName.equals(superClassname)) {
				sb.append(superClass);
			} else {
				sb.append(superClassname);
				addImport(packageName, imports, superClass);
			}
		}

		sb.append(" {"); //$NON-NLS-1$
		sb.append(lineSeparator);

		sb.append(indentation);
		sb.append(lineSeparator);

		sb.append("}"); //$NON-NLS-1$
		sb.append(lineSeparator);
		return sb.toString();
	}

	/** Create the SARL code that corresponds to a skill with the given properties.
	 *
	 * @param packageName - the name of the package where the skill is created.
	 * @param skillName - name of the event type.
	 * @param superClass - the name of the super type.
	 * @param capacityTypes - the implemented capacities.
	 * @param imports - the list of the types to put into the import section for supporting
	 *                  the generated code. If this parameter is <code>null</code>, the
	 *                  list is not filled.
	 * @param indentation - identation string.
	 * @param lineSeparator - line separator.
	 * @param generateActions - indicates if the actions should be generated.
	 * @return the SARL code for the skill.
	 */
	public static String createSkillContent(
			String packageName,
			String skillName, String superClass,
			Collection<String> capacityTypes,
			Set<String> imports,
			String indentation, String lineSeparator,
			boolean generateActions) {
		StringBuilder sb = new StringBuilder();
		sb.append(SARLKeywords.SKILL);
		sb.append(' ');
		sb.append(skillName);

		if (!Strings.isNullOrEmpty(superClass) && !Skill.class.getName().equals(superClass)) {
			sb.append(' ');
			sb.append(SARLKeywords.EXTENDS);
			sb.append(' ');
			String superClassname = stripPackage(superClass);
			if (skillName.equals(superClassname)) {
				sb.append(superClass);
			} else {
				sb.append(superClassname);
				addImport(packageName, imports, superClass);
			}
		}

		if (!capacityTypes.isEmpty()) {
			sb.append(' ');
			sb.append(SARLKeywords.IMPLEMENTS);
			sb.append(' ');
			boolean addComa = false;
			for (String capacityType : capacityTypes) {
				if (addComa) {
					sb.append(", "); //$NON-NLS-1$
				}
				String superClassname = stripPackage(capacityType);
				if (skillName.equals(superClassname)) {
					sb.append(capacityType);
				} else {
					sb.append(superClassname);
					addImport(packageName, imports, capacityType);
				}
				addComa = true;
			}
		}

		sb.append(" {"); //$NON-NLS-1$
		sb.append(lineSeparator);

		sb.append(indentation);
		sb.append(lineSeparator);

		sb.append("}"); //$NON-NLS-1$
		sb.append(lineSeparator);
		return sb.toString();
	}

	private static String stripPackage(Object object) {
		return object.toString().replaceAll("^(\\w+\\.)*", ""); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static boolean isInCurrentPackage(String currentPackage, String element) {
		Pattern pattern = Pattern.compile("^(?:\\w+\\.)+(\\w+)$"); //$NON-NLS-1$
		Matcher matcher = pattern.matcher(element);
		String packageName;
		if (matcher.matches()) {
			String name = matcher.group(1);
			packageName = element.substring(0, element.length() - name.length() - 1);
		} else {
			packageName = ""; //$NON-NLS-1$
		}
		if (!packageName.equals(currentPackage)) {
			return false;
		}
		return true;
	}

	private static void addImport(String currentPackage, Set<String> imports, String importElement) {
		if (imports != null && !isInCurrentPackage(currentPackage, importElement)) {
			imports.add(importElement);
		}
	}

}
