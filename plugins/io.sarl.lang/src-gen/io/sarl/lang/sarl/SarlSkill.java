/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Skill</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlSkill#getSuperTypes <em>Super Types</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlSkill#getImplementedTypes <em>Implemented Types</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlSkill#getFeatures <em>Features</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlSkill()
 * @model
 * @generated
 */
public interface SarlSkill extends XtendTypeDeclaration
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
   * @see io.sarl.lang.sarl.SarlPackage#getSarlSkill_SuperTypes()
   * @model containment="true"
   * @generated
   */
  EList<JvmParameterizedTypeReference> getSuperTypes();

  /**
   * Returns the value of the '<em><b>Implemented Types</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtext.common.types.JvmParameterizedTypeReference}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Implemented Types</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Implemented Types</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSarlSkill_ImplementedTypes()
   * @model containment="true"
   * @generated
   */
  EList<JvmParameterizedTypeReference> getImplementedTypes();

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
   * @see io.sarl.lang.sarl.SarlPackage#getSarlSkill_Features()
   * @model containment="true"
   * @generated
   */
  EList<XtendMember> getFeatures();

} // SarlSkill
