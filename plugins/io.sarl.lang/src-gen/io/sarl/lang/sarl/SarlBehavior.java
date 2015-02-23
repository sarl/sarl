/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Behavior</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlBehavior#getSuperTypes <em>Super Types</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlBehavior#getFeatures <em>Features</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlBehavior()
 * @model
 * @generated
 */
public interface SarlBehavior extends XtendTypeDeclaration
{
  /**
   * Returns the value of the '<em><b>Super Types</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtext.common.types.JvmParameterizedTypeReference}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Super Types</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Super Types</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSarlBehavior_SuperTypes()
   * @model containment="true"
   * @generated
   */
  EList<JvmParameterizedTypeReference> getSuperTypes();

  /**
   * Returns the value of the '<em><b>Features</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtend.core.xtend.XtendMember}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Features</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Features</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSarlBehavior_Features()
   * @model containment="true"
   * @generated
   */
  EList<XtendMember> getFeatures();

} // SarlBehavior
