/**
 */
package io.sarl.lang.sarl.impl;

import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.impl.XtendFactoryImpl;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the XtendFactory for SARL.
 * <!-- end-user-doc -->
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SarlXtendFactoryImpl extends XtendFactoryImpl {

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 */
	public SarlXtendFactoryImpl() {
		super();
	}
	
	@Override
	public XtendField createXtendField() {
		return new SarlFieldImplCustom();
	}

}
