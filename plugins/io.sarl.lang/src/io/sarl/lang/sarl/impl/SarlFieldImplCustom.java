/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.SarlEvent;

import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend.core.xtend.impl.XtendFieldImplCustom;
import org.eclipse.xtext.common.types.JvmVisibility;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the XtendField grammar element for SARL.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link #getDefaultVisibility() <em>Default visibility of fields in SARL elements</em>}</li>
 * </ul>
 * </p>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SarlFieldImplCustom extends XtendFieldImplCustom {

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SarlFieldImplCustom() {
		super();
	}

	@Override
	protected JvmVisibility getDefaultVisibility() {
		XtendTypeDeclaration declaration = getDeclaringType();
		if(declaration instanceof XtendInterface
			|| declaration instanceof XtendAnnotationType
			|| declaration instanceof SarlEvent) {
			return JvmVisibility.PUBLIC;
		}
		return JvmVisibility.PRIVATE;
	}

}
