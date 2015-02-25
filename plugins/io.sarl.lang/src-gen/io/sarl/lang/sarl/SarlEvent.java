/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Event</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlEvent#getExtends <em>Extends</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlEvent()
 * @model
 * @generated
 */
public interface SarlEvent extends XtendTypeDeclaration
{
  /**
   * Returns the value of the '<em><b>Extends</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtext.common.types.JvmParameterizedTypeReference}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Extends</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Extends</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSarlEvent_Extends()
   * @model containment="true"
   * @generated
   */
  EList<JvmParameterizedTypeReference> getExtends();

} // SarlEvent
