/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.xtend.core.xtend.XtendPackage;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see io.sarl.lang.sarl.SarlFactory
 * @model kind="package"
 * @generated
 */
public interface SarlPackage extends EPackage
{
  /**
   * The package name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNAME = "sarl";

  /**
   * The package namespace URI.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_URI = "http://www.sarl.io/lang/SARL";

  /**
   * The package namespace name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_PREFIX = "sarl";

  /**
   * The singleton instance of the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  SarlPackage eINSTANCE = io.sarl.lang.sarl.impl.SarlPackageImpl.init();

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlScriptImpl <em>Script</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlScriptImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlScript()
   * @generated
   */
  int SARL_SCRIPT = 0;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SCRIPT__NAME = 0;

  /**
   * The feature id for the '<em><b>Import Section</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SCRIPT__IMPORT_SECTION = 1;

  /**
   * The feature id for the '<em><b>Elements</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SCRIPT__ELEMENTS = 2;

  /**
   * The number of structural features of the '<em>Script</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SCRIPT_FEATURE_COUNT = 3;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlActionSignatureImpl <em>Action Signature</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlActionSignatureImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlActionSignature()
   * @generated
   */
  int SARL_ACTION_SIGNATURE = 1;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__ANNOTATIONS = XtendPackage.XTEND_MEMBER__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Annotation Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__ANNOTATION_INFO = XtendPackage.XTEND_MEMBER__ANNOTATION_INFO;

  /**
   * The feature id for the '<em><b>Modifiers</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__MODIFIERS = XtendPackage.XTEND_MEMBER__MODIFIERS;

  /**
   * The feature id for the '<em><b>Declaring Type</b></em>' container reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__DECLARING_TYPE = XtendPackage.XTEND_MEMBER__DECLARING_TYPE;

  /**
   * The feature id for the '<em><b>Type Parameters</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__TYPE_PARAMETERS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__NAME = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Parameters</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__PARAMETERS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Varargs</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__VARARGS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Return Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__RETURN_TYPE = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 4;

  /**
   * The feature id for the '<em><b>Create Extension Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__CREATE_EXTENSION_INFO = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 5;

  /**
   * The feature id for the '<em><b>Exceptions</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__EXCEPTIONS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 6;

  /**
   * The feature id for the '<em><b>Fired Events</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE__FIRED_EVENTS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 7;

  /**
   * The number of structural features of the '<em>Action Signature</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_SIGNATURE_FEATURE_COUNT = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 8;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlActionImpl <em>Action</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlActionImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlAction()
   * @generated
   */
  int SARL_ACTION = 2;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__ANNOTATIONS = XtendPackage.XTEND_MEMBER__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Annotation Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__ANNOTATION_INFO = XtendPackage.XTEND_MEMBER__ANNOTATION_INFO;

  /**
   * The feature id for the '<em><b>Modifiers</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__MODIFIERS = XtendPackage.XTEND_MEMBER__MODIFIERS;

  /**
   * The feature id for the '<em><b>Declaring Type</b></em>' container reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__DECLARING_TYPE = XtendPackage.XTEND_MEMBER__DECLARING_TYPE;

  /**
   * The feature id for the '<em><b>Type Parameters</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__TYPE_PARAMETERS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__NAME = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Parameters</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__PARAMETERS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Varargs</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__VARARGS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Return Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__RETURN_TYPE = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 4;

  /**
   * The feature id for the '<em><b>Create Extension Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__CREATE_EXTENSION_INFO = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 5;

  /**
   * The feature id for the '<em><b>Exceptions</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__EXCEPTIONS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 6;

  /**
   * The feature id for the '<em><b>Fired Events</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__FIRED_EVENTS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 7;

  /**
   * The feature id for the '<em><b>Expression</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION__EXPRESSION = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 8;

  /**
   * The number of structural features of the '<em>Action</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_ACTION_FEATURE_COUNT = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 9;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlBehaviorUnitImpl <em>Behavior Unit</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlBehaviorUnitImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlBehaviorUnit()
   * @generated
   */
  int SARL_BEHAVIOR_UNIT = 3;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR_UNIT__ANNOTATIONS = XtendPackage.XTEND_MEMBER__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Annotation Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR_UNIT__ANNOTATION_INFO = XtendPackage.XTEND_MEMBER__ANNOTATION_INFO;

  /**
   * The feature id for the '<em><b>Modifiers</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR_UNIT__MODIFIERS = XtendPackage.XTEND_MEMBER__MODIFIERS;

  /**
   * The feature id for the '<em><b>Declaring Type</b></em>' container reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR_UNIT__DECLARING_TYPE = XtendPackage.XTEND_MEMBER__DECLARING_TYPE;

  /**
   * The feature id for the '<em><b>Name</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR_UNIT__NAME = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Guard</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR_UNIT__GUARD = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Body</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR_UNIT__BODY = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Behavior Unit</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR_UNIT_FEATURE_COUNT = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlCapacityUsesImpl <em>Capacity Uses</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlCapacityUsesImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlCapacityUses()
   * @generated
   */
  int SARL_CAPACITY_USES = 4;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY_USES__ANNOTATIONS = XtendPackage.XTEND_MEMBER__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Annotation Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY_USES__ANNOTATION_INFO = XtendPackage.XTEND_MEMBER__ANNOTATION_INFO;

  /**
   * The feature id for the '<em><b>Modifiers</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY_USES__MODIFIERS = XtendPackage.XTEND_MEMBER__MODIFIERS;

  /**
   * The feature id for the '<em><b>Declaring Type</b></em>' container reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY_USES__DECLARING_TYPE = XtendPackage.XTEND_MEMBER__DECLARING_TYPE;

  /**
   * The feature id for the '<em><b>Capacities Used</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY_USES__CAPACITIES_USED = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Capacity Uses</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY_USES_FEATURE_COUNT = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlRequiredCapacityImpl <em>Required Capacity</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlRequiredCapacityImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlRequiredCapacity()
   * @generated
   */
  int SARL_REQUIRED_CAPACITY = 5;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_REQUIRED_CAPACITY__ANNOTATIONS = XtendPackage.XTEND_MEMBER__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Annotation Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_REQUIRED_CAPACITY__ANNOTATION_INFO = XtendPackage.XTEND_MEMBER__ANNOTATION_INFO;

  /**
   * The feature id for the '<em><b>Modifiers</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_REQUIRED_CAPACITY__MODIFIERS = XtendPackage.XTEND_MEMBER__MODIFIERS;

  /**
   * The feature id for the '<em><b>Declaring Type</b></em>' container reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_REQUIRED_CAPACITY__DECLARING_TYPE = XtendPackage.XTEND_MEMBER__DECLARING_TYPE;

  /**
   * The feature id for the '<em><b>Required Capacities</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_REQUIRED_CAPACITY__REQUIRED_CAPACITIES = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Required Capacity</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_REQUIRED_CAPACITY_FEATURE_COUNT = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlConstructorImpl <em>Constructor</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlConstructorImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlConstructor()
   * @generated
   */
  int SARL_CONSTRUCTOR = 6;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CONSTRUCTOR__ANNOTATIONS = XtendPackage.XTEND_MEMBER__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Annotation Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CONSTRUCTOR__ANNOTATION_INFO = XtendPackage.XTEND_MEMBER__ANNOTATION_INFO;

  /**
   * The feature id for the '<em><b>Modifiers</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CONSTRUCTOR__MODIFIERS = XtendPackage.XTEND_MEMBER__MODIFIERS;

  /**
   * The feature id for the '<em><b>Declaring Type</b></em>' container reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CONSTRUCTOR__DECLARING_TYPE = XtendPackage.XTEND_MEMBER__DECLARING_TYPE;

  /**
   * The feature id for the '<em><b>Type Parameters</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CONSTRUCTOR__TYPE_PARAMETERS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Parameters</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CONSTRUCTOR__PARAMETERS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Varargs</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CONSTRUCTOR__VARARGS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Exceptions</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CONSTRUCTOR__EXCEPTIONS = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Expression</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CONSTRUCTOR__EXPRESSION = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 4;

  /**
   * The number of structural features of the '<em>Constructor</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CONSTRUCTOR_FEATURE_COUNT = XtendPackage.XTEND_MEMBER_FEATURE_COUNT + 5;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlEventImpl <em>Event</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlEventImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlEvent()
   * @generated
   */
  int SARL_EVENT = 7;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_EVENT__ANNOTATIONS = XtendPackage.XTEND_TYPE_DECLARATION__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Annotation Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_EVENT__ANNOTATION_INFO = XtendPackage.XTEND_TYPE_DECLARATION__ANNOTATION_INFO;

  /**
   * The feature id for the '<em><b>Modifiers</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_EVENT__MODIFIERS = XtendPackage.XTEND_TYPE_DECLARATION__MODIFIERS;

  /**
   * The feature id for the '<em><b>Declaring Type</b></em>' container reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_EVENT__DECLARING_TYPE = XtendPackage.XTEND_TYPE_DECLARATION__DECLARING_TYPE;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_EVENT__NAME = XtendPackage.XTEND_TYPE_DECLARATION__NAME;

  /**
   * The feature id for the '<em><b>Members</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_EVENT__MEMBERS = XtendPackage.XTEND_TYPE_DECLARATION__MEMBERS;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_EVENT__SUPER_TYPES = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_EVENT__FEATURES = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Event</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_EVENT_FEATURE_COUNT = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlAgentImpl <em>Agent</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlAgentImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlAgent()
   * @generated
   */
  int SARL_AGENT = 8;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_AGENT__ANNOTATIONS = XtendPackage.XTEND_TYPE_DECLARATION__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Annotation Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_AGENT__ANNOTATION_INFO = XtendPackage.XTEND_TYPE_DECLARATION__ANNOTATION_INFO;

  /**
   * The feature id for the '<em><b>Modifiers</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_AGENT__MODIFIERS = XtendPackage.XTEND_TYPE_DECLARATION__MODIFIERS;

  /**
   * The feature id for the '<em><b>Declaring Type</b></em>' container reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_AGENT__DECLARING_TYPE = XtendPackage.XTEND_TYPE_DECLARATION__DECLARING_TYPE;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_AGENT__NAME = XtendPackage.XTEND_TYPE_DECLARATION__NAME;

  /**
   * The feature id for the '<em><b>Members</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_AGENT__MEMBERS = XtendPackage.XTEND_TYPE_DECLARATION__MEMBERS;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_AGENT__SUPER_TYPES = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_AGENT__FEATURES = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Agent</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_AGENT_FEATURE_COUNT = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlCapacityImpl <em>Capacity</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlCapacityImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlCapacity()
   * @generated
   */
  int SARL_CAPACITY = 9;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY__ANNOTATIONS = XtendPackage.XTEND_TYPE_DECLARATION__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Annotation Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY__ANNOTATION_INFO = XtendPackage.XTEND_TYPE_DECLARATION__ANNOTATION_INFO;

  /**
   * The feature id for the '<em><b>Modifiers</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY__MODIFIERS = XtendPackage.XTEND_TYPE_DECLARATION__MODIFIERS;

  /**
   * The feature id for the '<em><b>Declaring Type</b></em>' container reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY__DECLARING_TYPE = XtendPackage.XTEND_TYPE_DECLARATION__DECLARING_TYPE;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY__NAME = XtendPackage.XTEND_TYPE_DECLARATION__NAME;

  /**
   * The feature id for the '<em><b>Members</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY__MEMBERS = XtendPackage.XTEND_TYPE_DECLARATION__MEMBERS;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY__SUPER_TYPES = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY__FEATURES = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Capacity</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_CAPACITY_FEATURE_COUNT = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlBehaviorImpl <em>Behavior</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlBehaviorImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlBehavior()
   * @generated
   */
  int SARL_BEHAVIOR = 10;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR__ANNOTATIONS = XtendPackage.XTEND_TYPE_DECLARATION__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Annotation Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR__ANNOTATION_INFO = XtendPackage.XTEND_TYPE_DECLARATION__ANNOTATION_INFO;

  /**
   * The feature id for the '<em><b>Modifiers</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR__MODIFIERS = XtendPackage.XTEND_TYPE_DECLARATION__MODIFIERS;

  /**
   * The feature id for the '<em><b>Declaring Type</b></em>' container reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR__DECLARING_TYPE = XtendPackage.XTEND_TYPE_DECLARATION__DECLARING_TYPE;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR__NAME = XtendPackage.XTEND_TYPE_DECLARATION__NAME;

  /**
   * The feature id for the '<em><b>Members</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR__MEMBERS = XtendPackage.XTEND_TYPE_DECLARATION__MEMBERS;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR__SUPER_TYPES = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR__FEATURES = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Behavior</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_BEHAVIOR_FEATURE_COUNT = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlSkillImpl <em>Skill</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlSkillImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlSkill()
   * @generated
   */
  int SARL_SKILL = 11;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SKILL__ANNOTATIONS = XtendPackage.XTEND_TYPE_DECLARATION__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Annotation Info</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SKILL__ANNOTATION_INFO = XtendPackage.XTEND_TYPE_DECLARATION__ANNOTATION_INFO;

  /**
   * The feature id for the '<em><b>Modifiers</b></em>' attribute list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SKILL__MODIFIERS = XtendPackage.XTEND_TYPE_DECLARATION__MODIFIERS;

  /**
   * The feature id for the '<em><b>Declaring Type</b></em>' container reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SKILL__DECLARING_TYPE = XtendPackage.XTEND_TYPE_DECLARATION__DECLARING_TYPE;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SKILL__NAME = XtendPackage.XTEND_TYPE_DECLARATION__NAME;

  /**
   * The feature id for the '<em><b>Members</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SKILL__MEMBERS = XtendPackage.XTEND_TYPE_DECLARATION__MEMBERS;

  /**
   * The feature id for the '<em><b>Super Types</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SKILL__SUPER_TYPES = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Implemented Types</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SKILL__IMPLEMENTED_TYPES = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Features</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SKILL__FEATURES = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Skill</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_SKILL_FEATURE_COUNT = XtendPackage.XTEND_TYPE_DECLARATION_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link io.sarl.lang.sarl.impl.SarlFormalParameterImpl <em>Formal Parameter</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see io.sarl.lang.sarl.impl.SarlFormalParameterImpl
   * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlFormalParameter()
   * @generated
   */
  int SARL_FORMAL_PARAMETER = 12;

  /**
   * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_FORMAL_PARAMETER__ANNOTATIONS = XtendPackage.XTEND_PARAMETER__ANNOTATIONS;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_FORMAL_PARAMETER__NAME = XtendPackage.XTEND_PARAMETER__NAME;

  /**
   * The feature id for the '<em><b>Parameter Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_FORMAL_PARAMETER__PARAMETER_TYPE = XtendPackage.XTEND_PARAMETER__PARAMETER_TYPE;

  /**
   * The feature id for the '<em><b>Var Arg</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_FORMAL_PARAMETER__VAR_ARG = XtendPackage.XTEND_PARAMETER__VAR_ARG;

  /**
   * The feature id for the '<em><b>Extension</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_FORMAL_PARAMETER__EXTENSION = XtendPackage.XTEND_PARAMETER__EXTENSION;

  /**
   * The feature id for the '<em><b>Default Value</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_FORMAL_PARAMETER__DEFAULT_VALUE = XtendPackage.XTEND_PARAMETER_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Formal Parameter</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SARL_FORMAL_PARAMETER_FEATURE_COUNT = XtendPackage.XTEND_PARAMETER_FEATURE_COUNT + 1;


  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlScript <em>Script</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Script</em>'.
   * @see io.sarl.lang.sarl.SarlScript
   * @generated
   */
  EClass getSarlScript();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.SarlScript#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see io.sarl.lang.sarl.SarlScript#getName()
   * @see #getSarlScript()
   * @generated
   */
  EAttribute getSarlScript_Name();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlScript#getImportSection <em>Import Section</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Import Section</em>'.
   * @see io.sarl.lang.sarl.SarlScript#getImportSection()
   * @see #getSarlScript()
   * @generated
   */
  EReference getSarlScript_ImportSection();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlScript#getElements <em>Elements</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Elements</em>'.
   * @see io.sarl.lang.sarl.SarlScript#getElements()
   * @see #getSarlScript()
   * @generated
   */
  EReference getSarlScript_Elements();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlActionSignature <em>Action Signature</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Action Signature</em>'.
   * @see io.sarl.lang.sarl.SarlActionSignature
   * @generated
   */
  EClass getSarlActionSignature();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlActionSignature#getTypeParameters <em>Type Parameters</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Type Parameters</em>'.
   * @see io.sarl.lang.sarl.SarlActionSignature#getTypeParameters()
   * @see #getSarlActionSignature()
   * @generated
   */
  EReference getSarlActionSignature_TypeParameters();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.SarlActionSignature#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see io.sarl.lang.sarl.SarlActionSignature#getName()
   * @see #getSarlActionSignature()
   * @generated
   */
  EAttribute getSarlActionSignature_Name();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlActionSignature#getParameters <em>Parameters</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Parameters</em>'.
   * @see io.sarl.lang.sarl.SarlActionSignature#getParameters()
   * @see #getSarlActionSignature()
   * @generated
   */
  EReference getSarlActionSignature_Parameters();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.SarlActionSignature#isVarargs <em>Varargs</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Varargs</em>'.
   * @see io.sarl.lang.sarl.SarlActionSignature#isVarargs()
   * @see #getSarlActionSignature()
   * @generated
   */
  EAttribute getSarlActionSignature_Varargs();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlActionSignature#getReturnType <em>Return Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Return Type</em>'.
   * @see io.sarl.lang.sarl.SarlActionSignature#getReturnType()
   * @see #getSarlActionSignature()
   * @generated
   */
  EReference getSarlActionSignature_ReturnType();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlActionSignature#getCreateExtensionInfo <em>Create Extension Info</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Create Extension Info</em>'.
   * @see io.sarl.lang.sarl.SarlActionSignature#getCreateExtensionInfo()
   * @see #getSarlActionSignature()
   * @generated
   */
  EReference getSarlActionSignature_CreateExtensionInfo();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlActionSignature#getExceptions <em>Exceptions</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Exceptions</em>'.
   * @see io.sarl.lang.sarl.SarlActionSignature#getExceptions()
   * @see #getSarlActionSignature()
   * @generated
   */
  EReference getSarlActionSignature_Exceptions();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlActionSignature#getFiredEvents <em>Fired Events</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Fired Events</em>'.
   * @see io.sarl.lang.sarl.SarlActionSignature#getFiredEvents()
   * @see #getSarlActionSignature()
   * @generated
   */
  EReference getSarlActionSignature_FiredEvents();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlAction <em>Action</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Action</em>'.
   * @see io.sarl.lang.sarl.SarlAction
   * @generated
   */
  EClass getSarlAction();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlAction#getTypeParameters <em>Type Parameters</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Type Parameters</em>'.
   * @see io.sarl.lang.sarl.SarlAction#getTypeParameters()
   * @see #getSarlAction()
   * @generated
   */
  EReference getSarlAction_TypeParameters();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.SarlAction#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see io.sarl.lang.sarl.SarlAction#getName()
   * @see #getSarlAction()
   * @generated
   */
  EAttribute getSarlAction_Name();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlAction#getParameters <em>Parameters</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Parameters</em>'.
   * @see io.sarl.lang.sarl.SarlAction#getParameters()
   * @see #getSarlAction()
   * @generated
   */
  EReference getSarlAction_Parameters();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.SarlAction#isVarargs <em>Varargs</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Varargs</em>'.
   * @see io.sarl.lang.sarl.SarlAction#isVarargs()
   * @see #getSarlAction()
   * @generated
   */
  EAttribute getSarlAction_Varargs();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlAction#getReturnType <em>Return Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Return Type</em>'.
   * @see io.sarl.lang.sarl.SarlAction#getReturnType()
   * @see #getSarlAction()
   * @generated
   */
  EReference getSarlAction_ReturnType();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlAction#getCreateExtensionInfo <em>Create Extension Info</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Create Extension Info</em>'.
   * @see io.sarl.lang.sarl.SarlAction#getCreateExtensionInfo()
   * @see #getSarlAction()
   * @generated
   */
  EReference getSarlAction_CreateExtensionInfo();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlAction#getExceptions <em>Exceptions</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Exceptions</em>'.
   * @see io.sarl.lang.sarl.SarlAction#getExceptions()
   * @see #getSarlAction()
   * @generated
   */
  EReference getSarlAction_Exceptions();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlAction#getFiredEvents <em>Fired Events</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Fired Events</em>'.
   * @see io.sarl.lang.sarl.SarlAction#getFiredEvents()
   * @see #getSarlAction()
   * @generated
   */
  EReference getSarlAction_FiredEvents();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlAction#getExpression <em>Expression</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Expression</em>'.
   * @see io.sarl.lang.sarl.SarlAction#getExpression()
   * @see #getSarlAction()
   * @generated
   */
  EReference getSarlAction_Expression();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlBehaviorUnit <em>Behavior Unit</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Behavior Unit</em>'.
   * @see io.sarl.lang.sarl.SarlBehaviorUnit
   * @generated
   */
  EClass getSarlBehaviorUnit();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlBehaviorUnit#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Name</em>'.
   * @see io.sarl.lang.sarl.SarlBehaviorUnit#getName()
   * @see #getSarlBehaviorUnit()
   * @generated
   */
  EReference getSarlBehaviorUnit_Name();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlBehaviorUnit#getGuard <em>Guard</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Guard</em>'.
   * @see io.sarl.lang.sarl.SarlBehaviorUnit#getGuard()
   * @see #getSarlBehaviorUnit()
   * @generated
   */
  EReference getSarlBehaviorUnit_Guard();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlBehaviorUnit#getBody <em>Body</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Body</em>'.
   * @see io.sarl.lang.sarl.SarlBehaviorUnit#getBody()
   * @see #getSarlBehaviorUnit()
   * @generated
   */
  EReference getSarlBehaviorUnit_Body();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlCapacityUses <em>Capacity Uses</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Capacity Uses</em>'.
   * @see io.sarl.lang.sarl.SarlCapacityUses
   * @generated
   */
  EClass getSarlCapacityUses();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlCapacityUses#getCapacitiesUsed <em>Capacities Used</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Capacities Used</em>'.
   * @see io.sarl.lang.sarl.SarlCapacityUses#getCapacitiesUsed()
   * @see #getSarlCapacityUses()
   * @generated
   */
  EReference getSarlCapacityUses_CapacitiesUsed();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlRequiredCapacity <em>Required Capacity</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Required Capacity</em>'.
   * @see io.sarl.lang.sarl.SarlRequiredCapacity
   * @generated
   */
  EClass getSarlRequiredCapacity();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlRequiredCapacity#getRequiredCapacities <em>Required Capacities</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Required Capacities</em>'.
   * @see io.sarl.lang.sarl.SarlRequiredCapacity#getRequiredCapacities()
   * @see #getSarlRequiredCapacity()
   * @generated
   */
  EReference getSarlRequiredCapacity_RequiredCapacities();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlConstructor <em>Constructor</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Constructor</em>'.
   * @see io.sarl.lang.sarl.SarlConstructor
   * @generated
   */
  EClass getSarlConstructor();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlConstructor#getTypeParameters <em>Type Parameters</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Type Parameters</em>'.
   * @see io.sarl.lang.sarl.SarlConstructor#getTypeParameters()
   * @see #getSarlConstructor()
   * @generated
   */
  EReference getSarlConstructor_TypeParameters();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlConstructor#getParameters <em>Parameters</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Parameters</em>'.
   * @see io.sarl.lang.sarl.SarlConstructor#getParameters()
   * @see #getSarlConstructor()
   * @generated
   */
  EReference getSarlConstructor_Parameters();

  /**
   * Returns the meta object for the attribute '{@link io.sarl.lang.sarl.SarlConstructor#isVarargs <em>Varargs</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Varargs</em>'.
   * @see io.sarl.lang.sarl.SarlConstructor#isVarargs()
   * @see #getSarlConstructor()
   * @generated
   */
  EAttribute getSarlConstructor_Varargs();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlConstructor#getExceptions <em>Exceptions</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Exceptions</em>'.
   * @see io.sarl.lang.sarl.SarlConstructor#getExceptions()
   * @see #getSarlConstructor()
   * @generated
   */
  EReference getSarlConstructor_Exceptions();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlConstructor#getExpression <em>Expression</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Expression</em>'.
   * @see io.sarl.lang.sarl.SarlConstructor#getExpression()
   * @see #getSarlConstructor()
   * @generated
   */
  EReference getSarlConstructor_Expression();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlEvent <em>Event</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Event</em>'.
   * @see io.sarl.lang.sarl.SarlEvent
   * @generated
   */
  EClass getSarlEvent();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlEvent#getSuperTypes <em>Super Types</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Super Types</em>'.
   * @see io.sarl.lang.sarl.SarlEvent#getSuperTypes()
   * @see #getSarlEvent()
   * @generated
   */
  EReference getSarlEvent_SuperTypes();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlEvent#getFeatures <em>Features</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Features</em>'.
   * @see io.sarl.lang.sarl.SarlEvent#getFeatures()
   * @see #getSarlEvent()
   * @generated
   */
  EReference getSarlEvent_Features();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlAgent <em>Agent</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Agent</em>'.
   * @see io.sarl.lang.sarl.SarlAgent
   * @generated
   */
  EClass getSarlAgent();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlAgent#getSuperTypes <em>Super Types</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Super Types</em>'.
   * @see io.sarl.lang.sarl.SarlAgent#getSuperTypes()
   * @see #getSarlAgent()
   * @generated
   */
  EReference getSarlAgent_SuperTypes();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlAgent#getFeatures <em>Features</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Features</em>'.
   * @see io.sarl.lang.sarl.SarlAgent#getFeatures()
   * @see #getSarlAgent()
   * @generated
   */
  EReference getSarlAgent_Features();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlCapacity <em>Capacity</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Capacity</em>'.
   * @see io.sarl.lang.sarl.SarlCapacity
   * @generated
   */
  EClass getSarlCapacity();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlCapacity#getSuperTypes <em>Super Types</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Super Types</em>'.
   * @see io.sarl.lang.sarl.SarlCapacity#getSuperTypes()
   * @see #getSarlCapacity()
   * @generated
   */
  EReference getSarlCapacity_SuperTypes();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlCapacity#getFeatures <em>Features</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Features</em>'.
   * @see io.sarl.lang.sarl.SarlCapacity#getFeatures()
   * @see #getSarlCapacity()
   * @generated
   */
  EReference getSarlCapacity_Features();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlBehavior <em>Behavior</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Behavior</em>'.
   * @see io.sarl.lang.sarl.SarlBehavior
   * @generated
   */
  EClass getSarlBehavior();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlBehavior#getSuperTypes <em>Super Types</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Super Types</em>'.
   * @see io.sarl.lang.sarl.SarlBehavior#getSuperTypes()
   * @see #getSarlBehavior()
   * @generated
   */
  EReference getSarlBehavior_SuperTypes();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlBehavior#getFeatures <em>Features</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Features</em>'.
   * @see io.sarl.lang.sarl.SarlBehavior#getFeatures()
   * @see #getSarlBehavior()
   * @generated
   */
  EReference getSarlBehavior_Features();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlSkill <em>Skill</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Skill</em>'.
   * @see io.sarl.lang.sarl.SarlSkill
   * @generated
   */
  EClass getSarlSkill();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlSkill#getSuperTypes <em>Super Types</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Super Types</em>'.
   * @see io.sarl.lang.sarl.SarlSkill#getSuperTypes()
   * @see #getSarlSkill()
   * @generated
   */
  EReference getSarlSkill_SuperTypes();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlSkill#getImplementedTypes <em>Implemented Types</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Implemented Types</em>'.
   * @see io.sarl.lang.sarl.SarlSkill#getImplementedTypes()
   * @see #getSarlSkill()
   * @generated
   */
  EReference getSarlSkill_ImplementedTypes();

  /**
   * Returns the meta object for the containment reference list '{@link io.sarl.lang.sarl.SarlSkill#getFeatures <em>Features</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Features</em>'.
   * @see io.sarl.lang.sarl.SarlSkill#getFeatures()
   * @see #getSarlSkill()
   * @generated
   */
  EReference getSarlSkill_Features();

  /**
   * Returns the meta object for class '{@link io.sarl.lang.sarl.SarlFormalParameter <em>Formal Parameter</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Formal Parameter</em>'.
   * @see io.sarl.lang.sarl.SarlFormalParameter
   * @generated
   */
  EClass getSarlFormalParameter();

  /**
   * Returns the meta object for the containment reference '{@link io.sarl.lang.sarl.SarlFormalParameter#getDefaultValue <em>Default Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Default Value</em>'.
   * @see io.sarl.lang.sarl.SarlFormalParameter#getDefaultValue()
   * @see #getSarlFormalParameter()
   * @generated
   */
  EReference getSarlFormalParameter_DefaultValue();

  /**
   * Returns the factory that creates the instances of the model.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the factory that creates the instances of the model.
   * @generated
   */
  SarlFactory getSarlFactory();

  /**
   * <!-- begin-user-doc -->
   * Defines literals for the meta objects that represent
   * <ul>
   *   <li>each class,</li>
   *   <li>each feature of each class,</li>
   *   <li>each enum,</li>
   *   <li>and each data type</li>
   * </ul>
   * <!-- end-user-doc -->
   * @generated
   */
  interface Literals
  {
    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlScriptImpl <em>Script</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlScriptImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlScript()
     * @generated
     */
    EClass SARL_SCRIPT = eINSTANCE.getSarlScript();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute SARL_SCRIPT__NAME = eINSTANCE.getSarlScript_Name();

    /**
     * The meta object literal for the '<em><b>Import Section</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_SCRIPT__IMPORT_SECTION = eINSTANCE.getSarlScript_ImportSection();

    /**
     * The meta object literal for the '<em><b>Elements</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_SCRIPT__ELEMENTS = eINSTANCE.getSarlScript_Elements();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlActionSignatureImpl <em>Action Signature</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlActionSignatureImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlActionSignature()
     * @generated
     */
    EClass SARL_ACTION_SIGNATURE = eINSTANCE.getSarlActionSignature();

    /**
     * The meta object literal for the '<em><b>Type Parameters</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION_SIGNATURE__TYPE_PARAMETERS = eINSTANCE.getSarlActionSignature_TypeParameters();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute SARL_ACTION_SIGNATURE__NAME = eINSTANCE.getSarlActionSignature_Name();

    /**
     * The meta object literal for the '<em><b>Parameters</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION_SIGNATURE__PARAMETERS = eINSTANCE.getSarlActionSignature_Parameters();

    /**
     * The meta object literal for the '<em><b>Varargs</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute SARL_ACTION_SIGNATURE__VARARGS = eINSTANCE.getSarlActionSignature_Varargs();

    /**
     * The meta object literal for the '<em><b>Return Type</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION_SIGNATURE__RETURN_TYPE = eINSTANCE.getSarlActionSignature_ReturnType();

    /**
     * The meta object literal for the '<em><b>Create Extension Info</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION_SIGNATURE__CREATE_EXTENSION_INFO = eINSTANCE.getSarlActionSignature_CreateExtensionInfo();

    /**
     * The meta object literal for the '<em><b>Exceptions</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION_SIGNATURE__EXCEPTIONS = eINSTANCE.getSarlActionSignature_Exceptions();

    /**
     * The meta object literal for the '<em><b>Fired Events</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION_SIGNATURE__FIRED_EVENTS = eINSTANCE.getSarlActionSignature_FiredEvents();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlActionImpl <em>Action</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlActionImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlAction()
     * @generated
     */
    EClass SARL_ACTION = eINSTANCE.getSarlAction();

    /**
     * The meta object literal for the '<em><b>Type Parameters</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION__TYPE_PARAMETERS = eINSTANCE.getSarlAction_TypeParameters();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute SARL_ACTION__NAME = eINSTANCE.getSarlAction_Name();

    /**
     * The meta object literal for the '<em><b>Parameters</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION__PARAMETERS = eINSTANCE.getSarlAction_Parameters();

    /**
     * The meta object literal for the '<em><b>Varargs</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute SARL_ACTION__VARARGS = eINSTANCE.getSarlAction_Varargs();

    /**
     * The meta object literal for the '<em><b>Return Type</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION__RETURN_TYPE = eINSTANCE.getSarlAction_ReturnType();

    /**
     * The meta object literal for the '<em><b>Create Extension Info</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION__CREATE_EXTENSION_INFO = eINSTANCE.getSarlAction_CreateExtensionInfo();

    /**
     * The meta object literal for the '<em><b>Exceptions</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION__EXCEPTIONS = eINSTANCE.getSarlAction_Exceptions();

    /**
     * The meta object literal for the '<em><b>Fired Events</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION__FIRED_EVENTS = eINSTANCE.getSarlAction_FiredEvents();

    /**
     * The meta object literal for the '<em><b>Expression</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_ACTION__EXPRESSION = eINSTANCE.getSarlAction_Expression();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlBehaviorUnitImpl <em>Behavior Unit</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlBehaviorUnitImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlBehaviorUnit()
     * @generated
     */
    EClass SARL_BEHAVIOR_UNIT = eINSTANCE.getSarlBehaviorUnit();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_BEHAVIOR_UNIT__NAME = eINSTANCE.getSarlBehaviorUnit_Name();

    /**
     * The meta object literal for the '<em><b>Guard</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_BEHAVIOR_UNIT__GUARD = eINSTANCE.getSarlBehaviorUnit_Guard();

    /**
     * The meta object literal for the '<em><b>Body</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_BEHAVIOR_UNIT__BODY = eINSTANCE.getSarlBehaviorUnit_Body();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlCapacityUsesImpl <em>Capacity Uses</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlCapacityUsesImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlCapacityUses()
     * @generated
     */
    EClass SARL_CAPACITY_USES = eINSTANCE.getSarlCapacityUses();

    /**
     * The meta object literal for the '<em><b>Capacities Used</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_CAPACITY_USES__CAPACITIES_USED = eINSTANCE.getSarlCapacityUses_CapacitiesUsed();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlRequiredCapacityImpl <em>Required Capacity</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlRequiredCapacityImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlRequiredCapacity()
     * @generated
     */
    EClass SARL_REQUIRED_CAPACITY = eINSTANCE.getSarlRequiredCapacity();

    /**
     * The meta object literal for the '<em><b>Required Capacities</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_REQUIRED_CAPACITY__REQUIRED_CAPACITIES = eINSTANCE.getSarlRequiredCapacity_RequiredCapacities();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlConstructorImpl <em>Constructor</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlConstructorImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlConstructor()
     * @generated
     */
    EClass SARL_CONSTRUCTOR = eINSTANCE.getSarlConstructor();

    /**
     * The meta object literal for the '<em><b>Type Parameters</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_CONSTRUCTOR__TYPE_PARAMETERS = eINSTANCE.getSarlConstructor_TypeParameters();

    /**
     * The meta object literal for the '<em><b>Parameters</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_CONSTRUCTOR__PARAMETERS = eINSTANCE.getSarlConstructor_Parameters();

    /**
     * The meta object literal for the '<em><b>Varargs</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute SARL_CONSTRUCTOR__VARARGS = eINSTANCE.getSarlConstructor_Varargs();

    /**
     * The meta object literal for the '<em><b>Exceptions</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_CONSTRUCTOR__EXCEPTIONS = eINSTANCE.getSarlConstructor_Exceptions();

    /**
     * The meta object literal for the '<em><b>Expression</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_CONSTRUCTOR__EXPRESSION = eINSTANCE.getSarlConstructor_Expression();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlEventImpl <em>Event</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlEventImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlEvent()
     * @generated
     */
    EClass SARL_EVENT = eINSTANCE.getSarlEvent();

    /**
     * The meta object literal for the '<em><b>Super Types</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_EVENT__SUPER_TYPES = eINSTANCE.getSarlEvent_SuperTypes();

    /**
     * The meta object literal for the '<em><b>Features</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_EVENT__FEATURES = eINSTANCE.getSarlEvent_Features();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlAgentImpl <em>Agent</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlAgentImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlAgent()
     * @generated
     */
    EClass SARL_AGENT = eINSTANCE.getSarlAgent();

    /**
     * The meta object literal for the '<em><b>Super Types</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_AGENT__SUPER_TYPES = eINSTANCE.getSarlAgent_SuperTypes();

    /**
     * The meta object literal for the '<em><b>Features</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_AGENT__FEATURES = eINSTANCE.getSarlAgent_Features();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlCapacityImpl <em>Capacity</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlCapacityImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlCapacity()
     * @generated
     */
    EClass SARL_CAPACITY = eINSTANCE.getSarlCapacity();

    /**
     * The meta object literal for the '<em><b>Super Types</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_CAPACITY__SUPER_TYPES = eINSTANCE.getSarlCapacity_SuperTypes();

    /**
     * The meta object literal for the '<em><b>Features</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_CAPACITY__FEATURES = eINSTANCE.getSarlCapacity_Features();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlBehaviorImpl <em>Behavior</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlBehaviorImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlBehavior()
     * @generated
     */
    EClass SARL_BEHAVIOR = eINSTANCE.getSarlBehavior();

    /**
     * The meta object literal for the '<em><b>Super Types</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_BEHAVIOR__SUPER_TYPES = eINSTANCE.getSarlBehavior_SuperTypes();

    /**
     * The meta object literal for the '<em><b>Features</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_BEHAVIOR__FEATURES = eINSTANCE.getSarlBehavior_Features();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlSkillImpl <em>Skill</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlSkillImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlSkill()
     * @generated
     */
    EClass SARL_SKILL = eINSTANCE.getSarlSkill();

    /**
     * The meta object literal for the '<em><b>Super Types</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_SKILL__SUPER_TYPES = eINSTANCE.getSarlSkill_SuperTypes();

    /**
     * The meta object literal for the '<em><b>Implemented Types</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_SKILL__IMPLEMENTED_TYPES = eINSTANCE.getSarlSkill_ImplementedTypes();

    /**
     * The meta object literal for the '<em><b>Features</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_SKILL__FEATURES = eINSTANCE.getSarlSkill_Features();

    /**
     * The meta object literal for the '{@link io.sarl.lang.sarl.impl.SarlFormalParameterImpl <em>Formal Parameter</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see io.sarl.lang.sarl.impl.SarlFormalParameterImpl
     * @see io.sarl.lang.sarl.impl.SarlPackageImpl#getSarlFormalParameter()
     * @generated
     */
    EClass SARL_FORMAL_PARAMETER = eINSTANCE.getSarlFormalParameter();

    /**
     * The meta object literal for the '<em><b>Default Value</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SARL_FORMAL_PARAMETER__DEFAULT_VALUE = eINSTANCE.getSarlFormalParameter_DefaultValue();

  }

} //SarlPackage
