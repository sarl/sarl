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

package io.sarl.docs.doclet.j11.utils;

import static javax.lang.model.element.Modifier.ABSTRACT;
import static javax.lang.model.element.Modifier.FINAL;
import static javax.lang.model.element.Modifier.NATIVE;
import static javax.lang.model.element.Modifier.PRIVATE;
import static javax.lang.model.element.Modifier.PROTECTED;
import static javax.lang.model.element.Modifier.PUBLIC;
import static javax.lang.model.element.Modifier.STRICTFP;
import static javax.lang.model.element.Modifier.SYNCHRONIZED;
import static jdk.javadoc.internal.doclets.formats.html.LinkInfoImpl.Kind.EXECUTABLE_MEMBER_PARAM;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;

import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;

import com.google.common.base.Strings;
import jdk.javadoc.internal.doclets.formats.html.AnnotationTypeWriterImpl;
import jdk.javadoc.internal.doclets.formats.html.Contents;
import jdk.javadoc.internal.doclets.formats.html.HtmlConfiguration;
import jdk.javadoc.internal.doclets.formats.html.LinkInfoImpl;
import jdk.javadoc.internal.doclets.formats.html.SubWriterHolderWriter;
import jdk.javadoc.internal.doclets.toolkit.ClassWriter;
import jdk.javadoc.internal.doclets.toolkit.Content;
import jdk.javadoc.internal.doclets.toolkit.util.Utils;

import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Utilities for writers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.1
 */
public final class GeneralUtils {

	/** Name of the bundle's property for the label of an agent type.
	 */
	public static final String DOCLET_AGENT = "doclet.Agent"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of a behavior type.
	 */
	public static final String DOCLET_BEHAVIOR = "doclet.Behavior"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of a event type.
	 */
	public static final String DOCLET_EVENT = "doclet.Event"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of a capacity type.
	 */
	public static final String DOCLET_CAPACITY = "doclet.Capacity"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of a skill type.
	 */
	public static final String DOCLET_SKILL = "doclet.Skill"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of a space type.
	 */
	public static final String DOCLET_SPACE = "doclet.Space"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of an artifact type.
	 */
	public static final String DOCLET_ARTIFACT = "doclet.Artifact"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of an enumeration type.
	 */
	public static final String DOCLET_ENUMERATION = "doclet.Enumeration"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of an annotation type.
	 */
	public static final String DOCLET_ANNOTATION = "doclet.Annotation"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of an interface type.
	 */
	public static final String DOCLET_INTERFACE = "doclet.Interface"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of a class type.
	 */
	public static final String DOCLET_CLASS = "doclet.Class"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of an agent type.
	 */
	public static final String DOCLET_TYPE_NAME = "doclet.type.name"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of an optional formal parameter.
	 */
	public static final String DOCLET_OPTIONAL_FORMAL_PARAMETER = "doclet.optional.formal.parameter"; //$NON-NLS-1$

	/** Name of the bundle's property for the left label of an optional formal parameter.
	 */
	public static final String DOCLET_OPTIONAL_FORMAL_PARAMETER_LEFT = "doclet.optional.formal.parameter.left"; //$NON-NLS-1$

	/** Name of the bundle's property for the right label of an optional formal parameter.
	 */
	public static final String DOCLET_OPTIONAL_FORMAL_PARAMETER_RIGHT = "doclet.optional.formal.parameter.right"; //$NON-NLS-1$

	private GeneralUtils() {
		//
	}

	/** Read the resource bundle for this doclet.
	 *
	 * @param key the key.
	 * @param control the locale selection.
	 * @param args arguments.
	 * @return the text.
	 */
	@SafeVarargs
	public static String getText(String key, Locale control, String... args) {
        try {
            final ResourceBundle bundle = ResourceBundle.getBundle(
            		GeneralUtils.class.getPackageName() + ".messages",
            		control);
            final String pattern = bundle.getString(key);
            if (args != null && args.length > 0) {
            	return MessageFormat.format(pattern, (Object[]) args);
            }
            return pattern;
        } catch (Exception e) {
            return null;
        }
	}

	/** Compute the type header.
	 *
	 * @param header the previous header.
	 * @param typeElement the element.
	 * @param localControl the controller of the locale.
	 * @return the header.
	 */
	public static String getHeader(String header, TypeElement typeElement, Locale localControl) {
		final SarlElementType sarlElementTypeAnno = typeElement.getAnnotation(SarlElementType.class);
		if (sarlElementTypeAnno != null) {
			final int sarlElementType = sarlElementTypeAnno.value();
			final String sn = typeElement.getSimpleName().toString();
			return getHeader(header, sarlElementType, sn, localControl);
		}
		return header;
	}

	/** Compute the type header.
	 *
	 * @param header the previous header.
	 * @param elementType the type of element.
	 * @param simpleName the simple name of the element.
	 * @param localControl the controller of the locale.
	 * @return the header.
	 */
	public static String getHeader(String header, int elementType, String simpleName, Locale localControl) {
		final String label;
		switch (elementType) {
		case SarlPackage.SARL_AGENT:
			label = getText(DOCLET_AGENT, localControl, simpleName);
			break;
		case SarlPackage.SARL_EVENT:
			label = getText(DOCLET_EVENT, localControl, simpleName);
			break;
		case SarlPackage.SARL_BEHAVIOR:
			label = getText(DOCLET_BEHAVIOR, localControl, simpleName);
			break;
		case SarlPackage.SARL_CAPACITY:
			label = getText(DOCLET_CAPACITY, localControl, simpleName);
			break;
		case SarlPackage.SARL_SKILL:
			label = getText(DOCLET_SKILL, localControl, simpleName);
			break;
		case SarlPackage.SARL_SPACE:
			label = getText(DOCLET_SPACE, localControl, simpleName);
			break;
		case SarlPackage.SARL_ENUMERATION:
			label = getText(DOCLET_ENUMERATION, localControl, simpleName);
			break;
		case SarlPackage.SARL_ANNOTATION_TYPE:
			label = getText(DOCLET_ANNOTATION, localControl, simpleName);
			break;
		case SarlPackage.SARL_INTERFACE:
			label = getText(DOCLET_INTERFACE, localControl, simpleName);
			break;
		case SarlPackage.SARL_CLASS:
			label = getText(DOCLET_CLASS, localControl, simpleName);
			break;
		default:
			label = null;
		}
		if (!Strings.isNullOrEmpty(label)) {
			return label;
		}
		return header;
	}

	/** Reply the declaration keyword for the given type of element.
	 *
	 * @param typeElement the element.
	 * @param sarlKeywords the accessors to the SARL keywords.
	 * @return the keyword.
	 */
	public static String getSarlKeyword(TypeElement typeElement, SARLGrammarKeywordAccess sarlKeywords) {
		final SarlElementType sarlElementTypeAnno = typeElement.getAnnotation(SarlElementType.class);
		if (sarlElementTypeAnno != null) {
			final int sarlElementType = sarlElementTypeAnno.value();
			switch (sarlElementType) {
			case SarlPackage.SARL_AGENT:
				return sarlKeywords.getAgentKeyword();
			case SarlPackage.SARL_EVENT:
				return sarlKeywords.getEventKeyword();
			case SarlPackage.SARL_BEHAVIOR:
				return sarlKeywords.getBehaviorKeyword();
			case SarlPackage.SARL_CAPACITY:
				return sarlKeywords.getCapacityKeyword();
			case SarlPackage.SARL_SKILL:
				return sarlKeywords.getSkillKeyword();
			case SarlPackage.SARL_SPACE:
				return sarlKeywords.getSpaceKeyword();
			case SarlPackage.SARL_ARTIFACT:
				return sarlKeywords.getArtifactKeyword();
			case SarlPackage.SARL_ENUMERATION:
				return sarlKeywords.getEnumKeyword();
			case SarlPackage.SARL_ANNOTATION_TYPE:
				return sarlKeywords.getAnnotationKeyword();
			case SarlPackage.SARL_INTERFACE:
				return sarlKeywords.getInterfaceKeyword();
			case SarlPackage.SARL_CLASS:
				return sarlKeywords.getClassKeyword();
			default:
				//
			}
		}
		return null;
	}

	/** Reply the type for the type of SARL element.
	 *
	 * @param type the element.
	 * @return the type.
	 */
	public static int getSarlType(TypeElement type) {
		final SarlElementType sarlElementTypeAnno = type.getAnnotation(SarlElementType.class);
		if (sarlElementTypeAnno != null) {
			return sarlElementTypeAnno.value();
		}
		return -1;
	}

	/** Reply the type for the type of SARL element.
	 *
	 * @param type the element.
	 * @return the type.
	 */
	public static int getSarlType(Element type) {
		final SarlElementType sarlElementTypeAnno = type.getAnnotation(SarlElementType.class);
		if (sarlElementTypeAnno != null) {
			return sarlElementTypeAnno.value();
		}
		return -1;
	}

	/** Reply the super type for the type of SARL element.
	 *
	 * @param type the element.
	 * @return the super type.
	 */
	public static String getSarlSuperType(TypeMirror type) {
		final SarlElementType sarlElementTypeAnno = type.getAnnotation(SarlElementType.class);
		if (sarlElementTypeAnno != null) {
			final int sarlElementType = sarlElementTypeAnno.value();
			switch (sarlElementType) {
			case SarlPackage.SARL_AGENT:
				return Agent.class.getName();
			case SarlPackage.SARL_EVENT:
				return Event.class.getName();
			case SarlPackage.SARL_BEHAVIOR:
				return Behavior.class.getName();
			case SarlPackage.SARL_CAPACITY:
				return Capacity.class.getName();
			case SarlPackage.SARL_SKILL:
				return Skill.class.getName();
			default:
				//
			}
		}
		return null;
	}

	/** Add field modifiers.
	 * Do not show up the "final" modifier; and add "package" modifier.
	 *
	 * @param member the associated element.
	 * @param htmltree the receiver of the HTML.
	 * @param writer associated writer.
	 * @param utils several utilities.
	 * @param sarlKeywords the accessors to the SARL keywords.
	 */
	public static void addFieldModifiers(Element member, Content htmltree, SubWriterHolderWriter writer,
			Utils utils, SARLGrammarKeywordAccess sarlKeywords) {
		final Set<Modifier> set = new TreeSet<>(member.getModifiers());

		// remove the ones we really don't need
		set.remove(NATIVE);
		set.remove(SYNCHRONIZED);
		set.remove(STRICTFP);
		set.remove(FINAL);

		// According to JLS, we should not be showing public modifier for
		// interface methods.
		assert utils.isField(member);

		boolean replaceByPackageModifier = false;
		if (!set.contains(PUBLIC) && !set.contains(PROTECTED) && !set.contains(PRIVATE)) {
			// Add the package modifier
			replaceByPackageModifier = true;
			set.add(PUBLIC);
		}

		if (writer instanceof ClassWriter
				&& (utils.isInterface(((ClassWriter) writer).getTypeElement())
					|| writer instanceof AnnotationTypeWriterImpl)) {
			// Remove the implicit abstract and public modifiers
			set.remove(ABSTRACT);
			set.remove(PUBLIC);
			set.remove(PROTECTED);
			set.remove(PRIVATE);
			replaceByPackageModifier = false;
		}

		if (!set.isEmpty()) {
			for (final Modifier mod : set) {
				if (Modifier.PUBLIC != mod || !replaceByPackageModifier) {
					htmltree.addContent(mod.toString());
				} else {
					htmltree.addContent(sarlKeywords.getPackageKeyword());
				}
				htmltree.addContent(Contents.SPACE);
			}
		}
	}

	/** Add method modifiers.
	 * Add "package" modifier.
	 *
	 * @param member the associated element.
	 * @param htmltree the receiver of the HTML.
	 * @param writer associated writer.
	 * @param utils several utilities.
	 * @param sarlKeywords the accessors to the SARL keywords.
	 */
	public static void addMethodModifiers(Element member, Content htmltree, SubWriterHolderWriter writer,
			Utils utils, SARLGrammarKeywordAccess sarlKeywords) {
		final Set<Modifier> set = new TreeSet<>(member.getModifiers());

		// remove the ones we really don't need
		set.remove(NATIVE);
		set.remove(SYNCHRONIZED);
		set.remove(STRICTFP);

		// According to JLS, we should not be showing public modifier for
		// interface methods.
		assert utils.isMethod(member);

		boolean replaceByPackageModifier = false;
		if (!set.contains(PUBLIC) && !set.contains(PROTECTED) && !set.contains(PRIVATE)) {
			// Add the package modifier
			replaceByPackageModifier = true;
			set.add(PUBLIC);
		}

		if (writer instanceof ClassWriter
				&& (utils.isInterface(((ClassWriter) writer).getTypeElement())
					|| writer instanceof AnnotationTypeWriterImpl)) {
			// Remove the implicit abstract and public modifiers
			set.remove(ABSTRACT);
			set.remove(PUBLIC);
			set.remove(PROTECTED);
			set.remove(PRIVATE);
			replaceByPackageModifier = false;
		}

		if (!set.isEmpty()) {
			for (final Modifier mod : set) {
				if (Modifier.PUBLIC != mod || !replaceByPackageModifier) {
					htmltree.addContent(mod.toString());
				} else {
					htmltree.addContent(sarlKeywords.getPackageKeyword());
				}
				htmltree.addContent(Contents.SPACE);
			}
		}
	}

	/** Add constructor modifiers.
	 * Add "package" modifier.
	 *
	 * @param member the associated element.
	 * @param htmltree the receiver of the HTML.
	 * @param writer associated writer.
	 * @param utils several utilities.
	 * @param sarlKeywords the accessors to the SARL keywords.
	 */
	public static void addConstructorModifiers(Element member, Content htmltree, SubWriterHolderWriter writer,
			Utils utils, SARLGrammarKeywordAccess sarlKeywords) {
		final Set<Modifier> set = new TreeSet<>(member.getModifiers());

		// remove the ones we really don't need
		set.remove(NATIVE);
		set.remove(SYNCHRONIZED);
		set.remove(STRICTFP);
		set.remove(ABSTRACT);

		// According to JLS, we should not be showing public modifier for
		// interface methods.
		assert utils.isConstructor(member);

		boolean replaceByPackageModifier = false;
		if (!set.contains(PUBLIC) && !set.contains(PROTECTED) && !set.contains(PRIVATE)) {
			// Add the package modifier
			replaceByPackageModifier = true;
			set.add(PUBLIC);
		}

		if (writer instanceof ClassWriter
				&& (utils.isInterface(((ClassWriter) writer).getTypeElement())
					|| writer instanceof AnnotationTypeWriterImpl)) {
			// Remove the implicit abstract and public modifiers
			set.remove(ABSTRACT);
			set.remove(PUBLIC);
			set.remove(PROTECTED);
			set.remove(PRIVATE);
			replaceByPackageModifier = false;
		}

		if (!set.isEmpty()) {
			for (final Modifier mod : set) {
				if (Modifier.PUBLIC != mod || !replaceByPackageModifier) {
					htmltree.addContent(mod.toString());
				} else {
					htmltree.addContent(sarlKeywords.getPackageKeyword());
				}
				htmltree.addContent(Contents.SPACE);
			}
		}
	}

	/** Add a formal parameter.
	 *
	 * @param member the element for which the parameter is added.
	 * @param param the definition of the formal parameter.
	 * @param paramName the name of the formal parameter.
	 * @param isOptional indicates if the parameter is optional.
	 * @param isVarArg indicates if the parameter is variadic.
	 * @param tree the HTML receiver.
	 * @param writer the writer.
	 * @param configuration the HTML configuration.
	 * @param sarlKeywords the accessors to the SARL keywords.
	 */
    public static void addParam(ExecutableElement member, VariableElement param, String paramName, boolean isOptional, boolean isVarArg, Content tree,
    		SubWriterHolderWriter writer, HtmlConfiguration configuration, SARLGrammarKeywordAccess sarlKeywords) {
        if (isOptional) {
        	tree.addContent(getText(DOCLET_OPTIONAL_FORMAL_PARAMETER_LEFT, configuration.getLocale()));
        }
        if(paramName.length() > 0) {
            tree.addContent(paramName);
            tree.addContent(Contents.SPACE);
            tree.addContent(sarlKeywords.getColonKeyword());
            tree.addContent(" "); //$NON-NLS-1$
        }
        final Content link = writer.getLink(new LinkInfoImpl(configuration, EXECUTABLE_MEMBER_PARAM,
                param.asType()).varargs(isVarArg));
        tree.addContent(link);
        if (isOptional) {
        	tree.addContent(getText(DOCLET_OPTIONAL_FORMAL_PARAMETER_RIGHT, configuration.getLocale()));
        }
    }

	/** Replies if the given element is of a given type.
	 *
	 * @param typeElement the element.
	 * @param superType the super types.
	 * @return {@code true} if the element is of the given type.
	 */
	@SafeVarargs
	public static boolean isOneOf(TypeElement typeElement, Integer... superType) {
		final int sarlType = getSarlType(typeElement);
		for (final int testedType : superType) {
			if (testedType == sarlType) {
				return true;
			}
		}
		return false;
	}

	/** Replies if the given element is of a given type.
	 *
	 * @param typeElement the element.
	 * @param superType the super types.
	 * @return {@code true} if the element is of the given type.
	 */
	@SafeVarargs
	public static boolean isOneOf(Element typeElement, Integer... superType) {
		final int sarlType = getSarlType(typeElement);
		for (final int testedType : superType) {
			if (testedType == sarlType) {
				return true;
			}
		}
		return false;
	}

	/** Replies a predicated that validate OOP elements.
	 *
	 * @return the predicate.
	 */
	public static Predicate<TypeElement> getOopSelector() {
		return it -> !GeneralUtils.isOneOf(it,
				SarlPackage.SARL_AGENT, SarlPackage.SARL_BEHAVIOR,
				SarlPackage.SARL_SKILL, SarlPackage.SARL_EVENT,
				SarlPackage.SARL_CAPACITY);
	}

	/** Replies a predicated that validate agent elements.
	 *
	 * @return the predicate.
	 */
	public static Predicate<TypeElement> getAgentSelector() {
		return it -> GeneralUtils.isOneOf(it, SarlPackage.SARL_AGENT);
	}

	/** Replies a predicated that validate capacity elements.
	 *
	 * @return the predicate.
	 */
	public static Predicate<TypeElement> getCapacitySelector() {
		return it -> GeneralUtils.isOneOf(it, SarlPackage.SARL_CAPACITY);
	}

	/** Replies a predicated that validate event elements.
	 *
	 * @return the predicate.
	 */
	public static Predicate<TypeElement> getEventSelector() {
		return it -> GeneralUtils.isOneOf(it, SarlPackage.SARL_EVENT);
	}

	/** Replies a predicated that validate behavior elements.
	 *
	 * @return the predicate.
	 */
	public static Predicate<TypeElement> getBehaviorSelector() {
		return it -> GeneralUtils.isOneOf(it, SarlPackage.SARL_BEHAVIOR);
	}

	/** Replies a predicated that validate skill elements.
	 *
	 * @return the predicate.
	 */
	public static Predicate<TypeElement> getSkillSelector() {
		return it -> GeneralUtils.isOneOf(it, SarlPackage.SARL_SKILL);
	}

}
