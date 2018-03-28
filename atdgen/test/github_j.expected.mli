(* Auto-generated from "github.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

type update_gist = Github_t.update_gist = {
  update_gist_description (*atd description *): string;
  update_gist_files (*atd files *): (string * update_gist) list
}

type json = Yojson.Safe.json

type wiki_page_action = Github_t.wiki_page_action

type wiki_page = Github_t.wiki_page = {
  wiki_page_name (*atd name *): string;
  wiki_page_title (*atd title *): string;
  wiki_page_action (*atd action *): wiki_page_action;
  wiki_page_sha (*atd sha *): string;
  wiki_page_html_url (*atd html_url *): string
}

type bool_as_string = Github_t.bool_as_string

type web_hook_config = Github_t.web_hook_config = {
  web_hook_config_url (*atd url *): string;
  web_hook_config_content_type (*atd content_type *): string option;
  web_hook_config_insecure_ssl (*atd insecure_ssl *): bool_as_string;
  web_hook_config_secret (*atd secret *): string option
}

type watch_action = Github_t.watch_action

type watch_event = Github_t.watch_event = {
  watch_event_action (*atd action *): watch_action
}

type user_info = Github_t.user_info = {
  user_info_name (*atd name *): string option;
  user_info_company (*atd company *): string option;
  user_info_blog (*atd blog *): string option;
  user_info_location (*atd location *): string option;
  user_info_email (*atd email *): string option;
  user_info_hireable (*atd hireable *): bool;
  user_info_bio (*atd bio *): string;
  user_info_public_repos (*atd public_repos *): int;
  user_info_public_gists (*atd public_gists *): int;
  user_info_followers (*atd followers *): int;
  user_info_following (*atd following *): int;
  user_info_created_at (*atd created_at *): string;
  user_info_updated_at (*atd updated_at *): string;
  user_info_html_url (*atd html_url *): string;
  user_info_login (*atd login *): string;
  user_info_id (*atd id *): Int64.t;
  user_info_url (*atd url *): string;
  user_info_avatar_url (*atd avatar_url *): string option
}

type user = Github_t.user = {
  user_login (*atd login *): string;
  user_id (*atd id *): Int64.t;
  user_url (*atd url *): string;
  user_avatar_url (*atd avatar_url *): string option
}

type update_release = Github_t.update_release = {
  update_release_tag_name (*atd tag_name *): string option;
  update_release_target_commitish (*atd target_commitish *): string option;
  update_release_name (*atd name *): string option;
  update_release_body (*atd body *): string option;
  update_release_draft (*atd draft *): bool option;
  update_release_prerelease (*atd prerelease *): bool option
}

type state = Github_t.state

type update_pull = Github_t.update_pull = {
  update_pull_title (*atd title *): string option;
  update_pull_body (*atd body *): string option;
  update_pull_state (*atd state *): state option;
  update_pull_base (*atd base *): string option
}

type update_milestone = Github_t.update_milestone = {
  update_milestone_title (*atd title *): string option;
  update_milestone_state (*atd state *): state option;
  update_milestone_description (*atd description *): string option;
  update_milestone_due_on (*atd due_on *): string option
}

type update_issue = Github_t.update_issue = {
  update_issue_title (*atd title *): string option;
  update_issue_body (*atd body *): string option;
  update_issue_state (*atd state *): state option;
  update_issue_assignee (*atd assignee *): string option;
  update_issue_milestone (*atd milestone *): int option;
  update_issue_labels (*atd labels *): string list option
}

type hook_config = Github_t.hook_config

type event_type = Github_t.event_type

type update_hook = Github_t.update_hook = {
  update_hook_config (*atd config *): hook_config;
  update_hook_events (*atd events *): event_type list option;
  update_hook_active (*atd active *): bool
}

type update_gist_file = Github_t.update_gist_file = {
  update_gist_file_content (*atd content *): string option;
  update_gist_file_name (*atd name *): string option
}

type pull_ref = Github_t.pull_ref = {
  pull_ref_url (*atd url *): string;
  pull_ref_html_url (*atd html_url *): string;
  pull_ref_diff_url (*atd diff_url *): string;
  pull_ref_patch_url (*atd patch_url *): string
}

type milestone = Github_t.milestone = {
  milestone_url (*atd url *): string;
  milestone_number (*atd number *): int;
  milestone_state (*atd state *): state;
  milestone_title (*atd title *): string;
  milestone_description (*atd description *): string;
  milestone_creator (*atd creator *): user option;
  milestone_open_issues (*atd open_issues *): int;
  milestone_closed_issues (*atd closed_issues *): int;
  milestone_created_at (*atd created_at *): string;
  milestone_due_on (*atd due_on *): string option
}

type label = Github_t.label = {
  label_url (*atd url *): string;
  label_name (*atd name *): string;
  label_color (*atd color *): string
}

type issue_sort = Github_t.issue_sort

type direction = Github_t.direction

type issue = Github_t.issue = {
  issue_url (*atd url *): string;
  issue_html_url (*atd html_url *): string;
  issue_number (*atd number *): int;
  issue_state (*atd state *): state;
  issue_title (*atd title *): string;
  issue_body (*atd body *): string;
  issue_user (*atd user *): user;
  issue_labels (*atd labels *): label list;
  issue_comments (*atd comments *): int;
  issue_created_at (*atd created_at *): string;
  issue_updated_at (*atd updated_at *): string;
  issue_closed_at (*atd closed_at *): string option;
  issue_milestone (*atd milestone *): milestone option;
  issue_sort (*atd sort *): issue_sort;
  issue_direction (*atd direction *): direction;
  issue_mentioned (*atd mentioned *): string list option;
  issue_pull_request (*atd pull_request *): pull_ref option
}

type timeline_source = Github_t.timeline_source = {
  timeline_source_id (*atd id *): int option;
  timeline_source_url (*atd url *): string option;
  timeline_source_actor (*atd actor *): user option;
  timeline_source_issue (*atd issue *): issue option
}

type timeline_action = Github_t.timeline_action

type issue_rename = Github_t.issue_rename = {
  issue_rename_from (*atd from *): string;
  issue_rename_to (*atd to *): string
}

type base_label = Github_t.base_label = {
  base_label_name (*atd name *): string;
  base_label_color (*atd color *): string
}

type timeline_event = Github_t.timeline_event = {
  timeline_event_id (*atd id *): int option;
  timeline_event_url (*atd url *): string option;
  timeline_event_actor (*atd actor *): user option;
  timeline_event_commit_id (*atd commit_id *): string option;
  timeline_event_event (*atd event *): timeline_action;
  timeline_event_created_at (*atd created_at *): string;
  timeline_event_label (*atd label *): base_label option;
  timeline_event_assignee (*atd assignee *): user option;
  timeline_event_milestone (*atd milestone *): milestone option;
  timeline_event_source (*atd source *): timeline_source option;
  timeline_event_rename (*atd rename *): issue_rename option
}

type timeline_events = Github_t.timeline_events

type change = Github_t.change = { change_from (*atd from *): string }

type ticket_changes = Github_t.ticket_changes = {
  ticket_changes_title (*atd title *): change option;
  ticket_changes_body (*atd body *): change option
}

type team = Github_t.team = {
  team_url (*atd url *): string;
  team_name (*atd name *): string;
  team_id (*atd id *): Int64.t
}

type teams = Github_t.teams

type team_permission = Github_t.team_permission

type org = Github_t.org = {
  org_login (*atd login *): string;
  org_id (*atd id *): Int64.t;
  org_url (*atd url *): string;
  org_avatar_url (*atd avatar_url *): string option
}

type team_info = Github_t.team_info = {
  team_info_permission (*atd permission *): team_permission;
  team_info_members_count (*atd members_count *): int;
  team_info_repos_count (*atd repos_count *): int;
  team_info_organization (*atd organization *): org;
  team_info_url (*atd url *): string;
  team_info_name (*atd name *): string;
  team_info_id (*atd id *): Int64.t
}

type team_infos = Github_t.team_infos

type team_add_info = Github_t.team_add_info = {
  team_add_info_slug (*atd slug *): string;
  team_add_info_permission (*atd permission *): team_permission;
  team_add_info_members_url (*atd members_url *): string;
  team_add_info_repositories_url (*atd repositories_url *): string;
  team_add_info_url (*atd url *): string;
  team_add_info_name (*atd name *): string;
  team_add_info_id (*atd id *): Int64.t
}

type repository = Github_t.repository = {
  repository_owner (*atd owner *): user;
  repository_full_name (*atd full_name *): string;
  repository_description (*atd description *): string option;
  repository_private (*atd private *): bool;
  repository_fork (*atd fork *): bool;
  repository_html_url (*atd html_url *): string;
  repository_clone_url (*atd clone_url *): string;
  repository_git_url (*atd git_url *): string;
  repository_ssh_url (*atd ssh_url *): string;
  repository_svn_url (*atd svn_url *): string;
  repository_mirror_url (*atd mirror_url *): string option;
  repository_homepage (*atd homepage *): string;
  repository_language (*atd language *): string option;
  repository_forks_count (*atd forks_count *): int;
  repository_subscribers_count (*atd subscribers_count *): int option;
  repository_stargazers_count (*atd stargazers_count *): int;
  repository_size (*atd size *): int;
  repository_default_branch (*atd default_branch *): string option;
  repository_open_issues_count (*atd open_issues_count *): int;
  repository_pushed_at (*atd pushed_at *): string option;
  repository_created_at (*atd created_at *): string;
  repository_updated_at (*atd updated_at *): string;
  repository_organization (*atd organization *): user option;
  repository_has_issues (*atd has_issues *): bool;
  repository_has_wiki (*atd has_wiki *): bool;
  repository_has_downloads (*atd has_downloads *): bool;
  repository_has_pages (*atd has_pages *): bool;
  repository_id (*atd id *): Int64.t;
  repository_name (*atd name *): string;
  repository_url (*atd url *): string
}

type team_add_event = Github_t.team_add_event = {
  team_add_event_team (*atd team *): team_add_info option;
  team_add_event_user (*atd user *): user option;
  team_add_event_repository (*atd repository *): repository option;
  team_add_event_organization (*atd organization *): org
}

type obj_type = Github_t.obj_type

type obj = Github_t.obj = {
  obj_ty (*atd ty *): obj_type;
  obj_sha (*atd sha *): string;
  obj_url (*atd url *): string
}

type info = Github_t.info = {
  info_date (*atd date *): string;
  info_email (*atd email *): string;
  info_name (*atd name *): string
}

type tag = Github_t.tag = {
  tag_obj (*atd obj *): obj;
  tag_url (*atd url *): string;
  tag_sha (*atd sha *): string;
  tag_tag (*atd tag *): string;
  tag_message (*atd message *): string;
  tag_tagger (*atd tagger *): info
}

type status_state = Github_t.status_state

type status = Github_t.status = {
  status_creator (*atd creator *): user;
  status_url (*atd url *): string;
  status_updated_at (*atd updated_at *): string;
  status_created_at (*atd created_at *): string;
  status_id (*atd id *): Int64.t;
  status_state (*atd state *): status_state;
  status_target_url (*atd target_url *): string option;
  status_description (*atd description *): string option;
  status_context (*atd context *): string option
}

type statuses = Github_t.statuses

type status_branch_commit = Github_t.status_branch_commit = {
  status_branch_commit_sha (*atd sha *): string;
  status_branch_commit_url (*atd url *): string
}

type status_branch = Github_t.status_branch = {
  status_branch_name (*atd name *): string;
  status_branch_commit (*atd commit *): status_branch_commit
}

type git_commit = Github_t.git_commit = {
  git_commit_url (*atd url *): string;
  git_commit_author (*atd author *): info;
  git_commit_message (*atd message *): string
}

type commit = Github_t.commit = {
  commit_url (*atd url *): string;
  commit_sha (*atd sha *): string;
  commit_git (*atd git *): git_commit;
  commit_author (*atd author *): user option;
  commit_committer (*atd committer *): user option
}

type status_event = Github_t.status_event = {
  status_event_sha (*atd sha *): string;
  status_event_target_url (*atd target_url *): string option;
  status_event_context (*atd context *): string option;
  status_event_description (*atd description *): string option;
  status_event_state (*atd state *): status_state;
  status_event_commit (*atd commit *): commit;
  status_event_branches (*atd branches *): status_branch list
}

type scope = Github_t.scope

type repositories = Github_t.repositories

type repository_search = Github_t.repository_search = {
  repository_search_total_count (*atd total_count *): int;
  repository_search_incomplete_results (*atd incomplete_results *): bool;
  repository_search_items (*atd items *): repositories
}

type repository_action = Github_t.repository_action

type repository_event = Github_t.repository_event = {
  repository_event_action (*atd action *): repository_action;
  repository_event_repository (*atd repository *): repository
}

type repo_commit = Github_t.repo_commit = {
  repo_commit_sha (*atd sha *): string;
  repo_commit_url (*atd url *): string
}

type repo_tag = Github_t.repo_tag = {
  repo_tag_name (*atd name *): string;
  repo_tag_commit (*atd commit *): repo_commit;
  repo_tag_zipball_url (*atd zipball_url *): string;
  repo_tag_tarball_url (*atd tarball_url *): string
}

type repo_tags = Github_t.repo_tags

type repo_issues_action = Github_t.repo_issues_action

type linked_user = Github_t.linked_user = {
  linked_user_html_url (*atd html_url *): string;
  linked_user_login (*atd login *): string;
  linked_user_id (*atd id *): Int64.t;
  linked_user_url (*atd url *): string;
  linked_user_avatar_url (*atd avatar_url *): string option
}

type repo_issues_event = Github_t.repo_issues_event = {
  repo_issues_event_issue (*atd issue *): issue;
  repo_issues_event_id (*atd id *): int;
  repo_issues_event_url (*atd url *): string;
  repo_issues_event_actor (*atd actor *): linked_user option;
  repo_issues_event_event (*atd event *): repo_issues_action;
  repo_issues_event_created_at (*atd created_at *): string;
  repo_issues_event_label (*atd label *): base_label option;
  repo_issues_event_assignee (*atd assignee *): linked_user option;
  repo_issues_event_assigner (*atd assigner *): linked_user option;
  repo_issues_event_milestone (*atd milestone *): milestone option;
  repo_issues_event_rename (*atd rename *): issue_rename option
}

type repo_issues_events = Github_t.repo_issues_events

type repo_issue_event = Github_t.repo_issue_event = {
  repo_issue_event_id (*atd id *): int;
  repo_issue_event_url (*atd url *): string;
  repo_issue_event_actor (*atd actor *): linked_user option;
  repo_issue_event_event (*atd event *): repo_issues_action;
  repo_issue_event_created_at (*atd created_at *): string;
  repo_issue_event_label (*atd label *): base_label option;
  repo_issue_event_assignee (*atd assignee *): linked_user option;
  repo_issue_event_assigner (*atd assigner *): linked_user option;
  repo_issue_event_milestone (*atd milestone *): milestone option;
  repo_issue_event_rename (*atd rename *): issue_rename option
}

type repo_issue_events = Github_t.repo_issue_events

type repo_branch = Github_t.repo_branch = {
  repo_branch_name (*atd name *): string;
  repo_branch_commit (*atd commit *): repo_commit
}

type repo_branches = Github_t.repo_branches

type repo = Github_t.repo = {
  repo_id (*atd id *): Int64.t;
  repo_name (*atd name *): string;
  repo_url (*atd url *): string
}

type release = Github_t.release = {
  release_id (*atd id *): Int64.t;
  release_tag_name (*atd tag_name *): string;
  release_target_commitish (*atd target_commitish *): string option;
  release_name (*atd name *): string option;
  release_body (*atd body *): string option;
  release_draft (*atd draft *): bool;
  release_prerelease (*atd prerelease *): bool;
  release_created_at (*atd created_at *): string;
  release_published_at (*atd published_at *): string;
  release_url (*atd url *): string;
  release_html_url (*atd html_url *): string;
  release_assets_url (*atd assets_url *): string;
  release_upload_url (*atd upload_url *): string
}

type releases = Github_t.releases

type release_repo = Github_t.release_repo = {
  release_repo_user (*atd user *): string;
  release_repo_repo (*atd repo *): string;
  release_repo_release (*atd release *): release
}

type release_repos = Github_t.release_repos

type release_action = Github_t.release_action

type release_event = Github_t.release_event = {
  release_event_action (*atd action *): release_action;
  release_event_release (*atd release *): release
}

type ref = Github_t.ref

type rate = Github_t.rate = {
  rate_limit (*atd limit *): int;
  rate_remaining (*atd remaining *): int;
  rate_reset (*atd reset *): float
}

type rate_resources = Github_t.rate_resources = {
  rate_resources_core (*atd core *): rate;
  rate_resources_search (*atd search *): rate
}

type rate_limit = Github_t.rate_limit = {
  rate_limit_resources (*atd resources *): rate_resources
}

type push_event_author = Github_t.push_event_author = {
  push_event_author_name (*atd name *): string;
  push_event_author_email (*atd email *): string
}

type push_event_hook_commit = Github_t.push_event_hook_commit = {
  push_event_hook_commit_id (*atd id *): string;
  push_event_hook_commit_tree_id (*atd tree_id *): string;
  push_event_hook_commit_url (*atd url *): string;
  push_event_hook_commit_message (*atd message *): string;
  push_event_hook_commit_author (*atd author *): push_event_author;
  push_event_hook_commit_distinct (*atd distinct *): bool
}

type push_event_hook = Github_t.push_event_hook = {
  push_event_hook_after (*atd after *): string;
  push_event_hook_created (*atd created *): bool;
  push_event_hook_deleted (*atd deleted *): bool;
  push_event_hook_forced (*atd forced *): bool;
  push_event_hook_commits (*atd commits *): push_event_hook_commit list;
  push_event_hook_head_commit (*atd head_commit *):
    push_event_hook_commit option;
  push_event_hook_ref (*atd ref *): string;
  push_event_hook_before (*atd before *): string
}

type push_event_commit_base = Github_t.push_event_commit_base = {
  push_event_commit_base_url (*atd url *): string;
  push_event_commit_base_message (*atd message *): string;
  push_event_commit_base_author (*atd author *): push_event_author;
  push_event_commit_base_distinct (*atd distinct *): bool
}

type push_event_commit = Github_t.push_event_commit = {
  push_event_commit_sha (*atd sha *): string;
  push_event_commit_url (*atd url *): string;
  push_event_commit_message (*atd message *): string;
  push_event_commit_author (*atd author *): push_event_author;
  push_event_commit_distinct (*atd distinct *): bool
}

type push_event_base = Github_t.push_event_base = {
  push_event_base_ref (*atd ref *): string;
  push_event_base_before (*atd before *): string
}

type push_event = Github_t.push_event = {
  push_event_head (*atd head *): string;
  push_event_size (*atd size *): int;
  push_event_commits (*atd commits *): push_event_commit list;
  push_event_ref (*atd ref *): string;
  push_event_before (*atd before *): string
}

type link = Github_t.link = { href: string }

type pull_links = Github_t.pull_links = {
  pull_self (*atd self *): link;
  pull_html (*atd html *): link;
  pull_comments (*atd comments *): link;
  pull_review_comments (*atd review_comments *): link
}

type branch = Github_t.branch = {
  branch_label (*atd label *): string;
  branch_ref (*atd ref *): string;
  branch_sha (*atd sha *): string;
  branch_user (*atd user *): user option;
  branch_repo (*atd repo *): repository option
}

type pull = Github_t.pull = {
  pull_issue_url (*atd issue_url *): string;
  pull_number (*atd number *): int;
  pull_state (*atd state *): state;
  pull_title (*atd title *): string;
  pull_body (*atd body *): string;
  pull_created_at (*atd created_at *): string;
  pull_updated_at (*atd updated_at *): string;
  pull_closed_at (*atd closed_at *): string option;
  pull_merged_at (*atd merged_at *): string option;
  pull_head (*atd head *): branch;
  pull_base (*atd base *): branch;
  pull_links (*atd links *): pull_links;
  pull_user (*atd user *): user;
  pull_url (*atd url *): string;
  pull_html_url (*atd html_url *): string;
  pull_diff_url (*atd diff_url *): string;
  pull_patch_url (*atd patch_url *): string
}

type pulls = Github_t.pulls

type body_changes = Github_t.body_changes = {
  body_changes_body (*atd body *): change option
}

type pull_request_review_comment_action =
  Github_t.pull_request_review_comment_action

type pull_request_review_comment = Github_t.pull_request_review_comment = {
  pull_request_review_comment_diff_hunk (*atd diff_hunk *): string;
  pull_request_review_comment_original_position (*atd original_position *):
    int;
  pull_request_review_comment_original_commit_id (*atd original_commit_id *):
    string;
  pull_request_review_comment_pull_request_url (*atd pull_request_url *):
    string;
  pull_request_review_comment_position (*atd position *): int option;
  pull_request_review_comment_line (*atd line *): int option;
  pull_request_review_comment_path (*atd path *): string option;
  pull_request_review_comment_commit_id (*atd commit_id *): string;
  pull_request_review_comment_id (*atd id *): Int64.t;
  pull_request_review_comment_url (*atd url *): string;
  pull_request_review_comment_html_url (*atd html_url *): string;
  pull_request_review_comment_body (*atd body *): string;
  pull_request_review_comment_user (*atd user *): user;
  pull_request_review_comment_created_at (*atd created_at *): string;
  pull_request_review_comment_updated_at (*atd updated_at *): string
}

type pull_request_review_comment_event =
  Github_t.pull_request_review_comment_event = {
  pull_request_review_comment_event_action (*atd action *):
    pull_request_review_comment_action;
  pull_request_review_comment_event_pull_request (*atd pull_request *): pull;
  pull_request_review_comment_event_comment (*atd comment *):
    pull_request_review_comment
}

type pull_request_action = Github_t.pull_request_action

type pull_request_event = Github_t.pull_request_event = {
  pull_request_event_action (*atd action *): pull_request_action;
  pull_request_event_number (*atd number *): int;
  pull_request_event_pull_request (*atd pull_request *): pull
}

type page_build_status = Github_t.page_build_status

type page_build_error = Github_t.page_build_error = {
  page_build_error_message (*atd message *): string option
}

type page_build = Github_t.page_build = {
  page_build_url (*atd url *): string;
  page_build_status (*atd status *): page_build_status option;
  page_build_error (*atd error *): page_build_error
}

type page_build_event = Github_t.page_build_event = {
  page_build_event_build (*atd build *): page_build
}

type orgs = Github_t.orgs

type organization = Github_t.organization = {
  organization_name (*atd name *): string;
  organization_company (*atd company *): string;
  organization_blog (*atd blog *): string;
  organization_location (*atd location *): string;
  organization_email (*atd email *): string;
  organization_public_repos (*atd public_repos *): int;
  organization_public_gists (*atd public_gists *): int;
  organization_followers (*atd followers *): int;
  organization_following (*atd following *): int;
  organization_html_url (*atd html_url *): string;
  organization_created_at (*atd created_at *): string;
  organization_login (*atd login *): string;
  organization_id (*atd id *): Int64.t;
  organization_url (*atd url *): string;
  organization_avatar_url (*atd avatar_url *): string option
}

type new_status = Github_t.new_status = {
  new_status_state (*atd state *): status_state;
  new_status_target_url (*atd target_url *): string option;
  new_status_description (*atd description *): string option;
  new_status_context (*atd context *): string option
}

type new_repo = Github_t.new_repo = {
  new_repo_name (*atd name *): string;
  new_repo_description (*atd description *): string;
  new_repo_homepage (*atd homepage *): string;
  new_repo_private (*atd private *): bool;
  new_repo_has_issues (*atd has_issues *): bool;
  new_repo_has_wiki (*atd has_wiki *): bool;
  new_repo_has_downloads (*atd has_downloads *): bool;
  new_repo_team_id (*atd team_id *): int;
  new_repo_auto_init (*atd auto_init *): bool;
  new_repo_gitignore_template (*atd gitignore_template *): string option;
  new_repo_license_template (*atd license_template *): string option
}

type new_release = Github_t.new_release = {
  new_release_tag_name (*atd tag_name *): string;
  new_release_target_commitish (*atd target_commitish *): string;
  new_release_name (*atd name *): string option;
  new_release_body (*atd body *): string option;
  new_release_draft (*atd draft *): bool;
  new_release_prerelease (*atd prerelease *): bool
}

type new_pull_issue = Github_t.new_pull_issue = {
  new_pull_issue_issue (*atd issue *): int;
  new_pull_issue_base (*atd base *): string;
  new_pull_issue_head (*atd head *): string
}

type new_pull = Github_t.new_pull = {
  new_pull_title (*atd title *): string;
  new_pull_body (*atd body *): string option;
  new_pull_base (*atd base *): string;
  new_pull_head (*atd head *): string
}

type new_milestone = Github_t.new_milestone = {
  new_milestone_title (*atd title *): string;
  new_milestone_state (*atd state *): state;
  new_milestone_description (*atd description *): string option;
  new_milestone_due_on (*atd due_on *): string option
}

type new_label = Github_t.new_label = {
  new_label_name (*atd name *): string;
  new_label_color (*atd color *): string
}

type new_issue_comment = Github_t.new_issue_comment = {
  new_issue_comment_body (*atd body *): string
}

type new_issue = Github_t.new_issue = {
  new_issue_title (*atd title *): string;
  new_issue_body (*atd body *): string option;
  new_issue_assignee (*atd assignee *): string option;
  new_issue_milestone (*atd milestone *): int option;
  new_issue_labels (*atd labels *): string list
}

type new_hook = Github_t.new_hook = {
  new_hook_config (*atd config *): hook_config;
  new_hook_events (*atd events *): event_type list;
  new_hook_active (*atd active *): bool
}

type new_gist_content = Github_t.new_gist_content = {
  new_gist_content (*atd content *): string
}

type new_gist_contents = Github_t.new_gist_contents

type new_gist = Github_t.new_gist = {
  new_gist_files (*atd files *): new_gist_contents;
  new_gist_description (*atd description *): string;
  new_gist_public (*atd public *): bool
}

type new_deploy_key = Github_t.new_deploy_key = {
  new_deploy_key_title (*atd title *): string;
  new_deploy_key_key (*atd key *): string
}

type milestones = Github_t.milestones

type milestone_sort = Github_t.milestone_sort

type error = Github_t.error = {
  error_resource (*atd resource *): string;
  error_field (*atd field *): string option;
  error_code (*atd code *): string;
  error_message (*atd message *): string option
}

type message = Github_t.message = {
  message_message (*atd message *): string;
  message_errors (*atd errors *): error list
}

type merge_request = Github_t.merge_request = {
  merge_commit_message (*atd commit_message *): string option
}

type merge = Github_t.merge = {
  merge_sha (*atd sha *): string option;
  merge_merged (*atd merged *): bool;
  merge_message (*atd message *): string
}

type member_action = Github_t.member_action

type member_event = Github_t.member_event = {
  member_event_action (*atd action *): member_action;
  member_event_member (*atd member *): linked_user
}

type linked_users = Github_t.linked_users

type labels = Github_t.labels

type label_names = Github_t.label_names

type issues_action = Github_t.issues_action

type issues_event = Github_t.issues_event = {
  issues_event_action (*atd action *): issues_action;
  issues_event_issue (*atd issue *): issue;
  issues_event_assignee (*atd assignee *): user_info option;
  issues_event_label (*atd label *): label option
}

type issues = Github_t.issues

type issue_comment = Github_t.issue_comment = {
  issue_comment_id (*atd id *): Int64.t;
  issue_comment_url (*atd url *): string;
  issue_comment_html_url (*atd html_url *): string;
  issue_comment_body (*atd body *): string;
  issue_comment_user (*atd user *): user;
  issue_comment_created_at (*atd created_at *): string;
  issue_comment_updated_at (*atd updated_at *): string
}

type issue_comments = Github_t.issue_comments

type issue_comment_action = Github_t.issue_comment_action

type issue_comment_event = Github_t.issue_comment_event = {
  issue_comment_event_action (*atd action *): issue_comment_action;
  issue_comment_event_issue (*atd issue *): issue;
  issue_comment_event_comment (*atd comment *): issue_comment
}

type hook = Github_t.hook = {
  hook_url (*atd url *): string;
  hook_updated_at (*atd updated_at *): string;
  hook_created_at (*atd created_at *): string;
  hook_events (*atd events *): event_type list;
  hook_active (*atd active *): bool;
  hook_config (*atd config *): hook_config;
  hook_id (*atd id *): Int64.t
}

type hooks = Github_t.hooks

type gollum_event = Github_t.gollum_event = {
  gollum_event_pages (*atd pages *): wiki_page list
}

type git_ref = Github_t.git_ref = {
  git_ref_name (*atd name *): string;
  git_ref_url (*atd url *): string;
  git_ref_obj (*atd obj *): obj
}

type git_refs = Github_t.git_refs

type gist_fork = Github_t.gist_fork = {
  gist_fork_user (*atd user *): user;
  gist_fork_url (*atd url *): string;
  gist_fork_id (*atd id *): Int64.t;
  gist_fork_created_at (*atd created_at *): string;
  gist_fork_updated_at (*atd updated_at *): string
}

type gist_file = Github_t.gist_file = {
  gist_file_size (*atd size *): int;
  gist_file_raw_url (*atd raw_url *): string;
  gist_file_ty (*atd ty *): string;
  gist_file_truncated (*atd truncated *): bool option;
  gist_file_language (*atd language *): string option;
  gist_file_content (*atd content *): string option
}

type gist_files = Github_t.gist_files

type change_status = Github_t.change_status = {
  change_status_deletions (*atd deletions *): int;
  change_status_additions (*atd additions *): int;
  change_status_total (*atd total *): int
}

type gist_commit = Github_t.gist_commit = {
  gist_commit_url (*atd url *): string;
  gist_commit_version (*atd version *): string;
  gist_commit_user (*atd user *): user;
  gist_commit_change_status (*atd change_status *): change_status;
  gist_commit_committed_at (*atd committed_at *): string
}

type gist_commits = Github_t.gist_commits

type gist = Github_t.gist = {
  gist_url (*atd url *): string;
  gist_forks_url (*atd forks_url *): string;
  gist_commits_url (*atd commits_url *): string;
  gist_id (*atd id *): string;
  gist_description (*atd description *): string option;
  gist_public (*atd public *): bool;
  gist_owner (*atd owner *): user;
  gist_user (*atd user *): string option;
  gist_files (*atd files *): gist_files;
  gist_comments (*atd comments *): int;
  gist_comments_url (*atd comments_url *): string;
  gist_html_url (*atd html_url *): string;
  gist_git_pull_url (*atd git_pull_url *): string;
  gist_git_push_url (*atd git_push_url *): string;
  gist_created_at (*atd created_at *): string;
  gist_updated_at (*atd updated_at *): string;
  gist_forks (*atd forks *): gist_fork list option;
  gist_history (*atd history *): gist_commits option
}

type gists = Github_t.gists

type gist_forks = Github_t.gist_forks

type fork_event = Github_t.fork_event = {
  fork_event_forkee (*atd forkee *): repository
}

type file = Github_t.file = {
  file_sha (*atd sha *): string option;
  file_filename (*atd filename *): string;
  file_status (*atd status *): string;
  file_additions (*atd additions *): int;
  file_deletions (*atd deletions *): int;
  file_changes (*atd changes *): int;
  file_blob_url (*atd blob_url *): string;
  file_raw_url (*atd raw_url *): string;
  file_patch (*atd patch *): string option
}

type files = Github_t.files

type delete_event = Github_t.delete_event = {
  delete_event_ref (*atd ref *): ref
}

type create_event = Github_t.create_event = {
  create_event_ref (*atd ref *): ref;
  create_event_master_branch (*atd master_branch *): string;
  create_event_description (*atd description *): string option
}

type commit_comment = Github_t.commit_comment = {
  commit_comment_position (*atd position *): int option;
  commit_comment_line (*atd line *): int option;
  commit_comment_path (*atd path *): string option;
  commit_comment_commit_id (*atd commit_id *): string;
  commit_comment_id (*atd id *): Int64.t;
  commit_comment_url (*atd url *): string;
  commit_comment_html_url (*atd html_url *): string;
  commit_comment_body (*atd body *): string;
  commit_comment_user (*atd user *): user;
  commit_comment_created_at (*atd created_at *): string;
  commit_comment_updated_at (*atd updated_at *): string
}

type commit_comment_event = Github_t.commit_comment_event = {
  commit_comment_event_comment (*atd comment *): commit_comment
}

type event_constr = Github_t.event_constr

type event = Github_t.event = {
  event_public (*atd public *): bool;
  event_payload (*atd payload *): event_constr;
  event_actor (*atd actor *): user;
  event_org (*atd org *): org option;
  event_created_at (*atd created_at *): string;
  event_repo (*atd repo *): repo;
  event_id (*atd id *): Int64.t
}

type events = Github_t.events

type event_hook_metadata = Github_t.event_hook_metadata = {
  event_hook_metadata_sender (*atd sender *): user;
  event_hook_metadata_organisation (*atd organisation *): org option;
  event_hook_metadata_created_at (*atd created_at *): string;
  event_hook_metadata_repository (*atd repository *): repository;
  event_hook_metadata_id (*atd id *): Int64.t
}

type event_hook_constr = Github_t.event_hook_constr

type emojis = Github_t.emojis

type deploy_key = Github_t.deploy_key = {
  deploy_key_id (*atd id *): Int64.t;
  deploy_key_key (*atd key *): string;
  deploy_key_url (*atd url *): string;
  deploy_key_title (*atd title *): string
}

type deploy_keys = Github_t.deploy_keys

type contribution_week = Github_t.contribution_week = {
  repo_contribution_week_w (*atd w *): int;
  repo_contribution_week_a (*atd a *): int;
  repo_contribution_week_d (*atd d *): int;
  repo_contribution_week_c (*atd c *): int
}

type contributor_stats = Github_t.contributor_stats = {
  repo_contributor_stats_author (*atd author *): user;
  repo_contributor_stats_total (*atd total *): int;
  repo_contributor_stats_weeks (*atd weeks *): contribution_week list
}

type contributors_stats = Github_t.contributors_stats

type contributor = Github_t.contributor = {
  contributor_contributions (*atd contributions *): int;
  contributor_html_url (*atd html_url *): string;
  contributor_login (*atd login *): string;
  contributor_id (*atd id *): Int64.t;
  contributor_url (*atd url *): string;
  contributor_avatar_url (*atd avatar_url *): string option
}

type contributors = Github_t.contributors

type commits = Github_t.commits

type comment = Github_t.comment = {
  comment_id (*atd id *): Int64.t;
  comment_url (*atd url *): string;
  comment_html_url (*atd html_url *): string;
  comment_body (*atd body *): string;
  comment_user (*atd user *): user;
  comment_created_at (*atd created_at *): string;
  comment_updated_at (*atd updated_at *): string
}

type base_status = Github_t.base_status = {
  base_status_url (*atd url *): string;
  base_status_updated_at (*atd updated_at *): string;
  base_status_created_at (*atd created_at *): string;
  base_status_id (*atd id *): Int64.t;
  base_status_state (*atd state *): status_state;
  base_status_target_url (*atd target_url *): string option;
  base_status_description (*atd description *): string option;
  base_status_context (*atd context *): string option
}

type base_statuses = Github_t.base_statuses

type combined_status = Github_t.combined_status = {
  combined_status_state (*atd state *): status_state;
  combined_status_sha (*atd sha *): string;
  combined_status_total_count (*atd total_count *): int;
  combined_status_statuses (*atd statuses *): base_statuses;
  combined_status_repository (*atd repository *): repo;
  combined_status_url (*atd url *): string;
  combined_status_commit_url (*atd commit_url *): string
}

type app = Github_t.app = {
  app_name (*atd name *): string;
  app_url (*atd url *): string
}

type auth = Github_t.auth = {
  auth_scopes (*atd scopes *): scope list;
  auth_token (*atd token *): string;
  auth_app (*atd app *): app;
  auth_url (*atd url *): string;
  auth_id (*atd id *): Int64.t;
  auth_note (*atd note *): string option;
  auth_note_url (*atd note_url *): string option
}

type auths = Github_t.auths

type auth_req = Github_t.auth_req = {
  auth_req_scopes (*atd scopes *): scope list;
  auth_req_note (*atd note *): string;
  auth_req_note_url (*atd note_url *): string option;
  auth_req_client_id (*atd client_id *): string option;
  auth_req_client_secret (*atd client_secret *): string option;
  auth_req_fingerprint (*atd fingerprint *): string option
}

val write_update_gist :
  Bi_outbuf.t -> update_gist -> unit
  (** Output a JSON value of type {!update_gist}. *)

val string_of_update_gist :
  ?len:int -> update_gist -> string
  (** Serialize a value of type {!update_gist}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_update_gist :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> update_gist
  (** Input JSON data of type {!update_gist}. *)

val update_gist_of_string :
  string -> update_gist
  (** Deserialize JSON data of type {!update_gist}. *)

val write_json :
  Bi_outbuf.t -> json -> unit
  (** Output a JSON value of type {!json}. *)

val string_of_json :
  ?len:int -> json -> string
  (** Serialize a value of type {!json}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_json :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> json
  (** Input JSON data of type {!json}. *)

val json_of_string :
  string -> json
  (** Deserialize JSON data of type {!json}. *)

val write_wiki_page_action :
  Bi_outbuf.t -> wiki_page_action -> unit
  (** Output a JSON value of type {!wiki_page_action}. *)

val string_of_wiki_page_action :
  ?len:int -> wiki_page_action -> string
  (** Serialize a value of type {!wiki_page_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_wiki_page_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> wiki_page_action
  (** Input JSON data of type {!wiki_page_action}. *)

val wiki_page_action_of_string :
  string -> wiki_page_action
  (** Deserialize JSON data of type {!wiki_page_action}. *)

val write_wiki_page :
  Bi_outbuf.t -> wiki_page -> unit
  (** Output a JSON value of type {!wiki_page}. *)

val string_of_wiki_page :
  ?len:int -> wiki_page -> string
  (** Serialize a value of type {!wiki_page}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_wiki_page :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> wiki_page
  (** Input JSON data of type {!wiki_page}. *)

val wiki_page_of_string :
  string -> wiki_page
  (** Deserialize JSON data of type {!wiki_page}. *)

val write_bool_as_string :
  Bi_outbuf.t -> bool_as_string -> unit
  (** Output a JSON value of type {!bool_as_string}. *)

val string_of_bool_as_string :
  ?len:int -> bool_as_string -> string
  (** Serialize a value of type {!bool_as_string}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_bool_as_string :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> bool_as_string
  (** Input JSON data of type {!bool_as_string}. *)

val bool_as_string_of_string :
  string -> bool_as_string
  (** Deserialize JSON data of type {!bool_as_string}. *)

val write_web_hook_config :
  Bi_outbuf.t -> web_hook_config -> unit
  (** Output a JSON value of type {!web_hook_config}. *)

val string_of_web_hook_config :
  ?len:int -> web_hook_config -> string
  (** Serialize a value of type {!web_hook_config}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_web_hook_config :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> web_hook_config
  (** Input JSON data of type {!web_hook_config}. *)

val web_hook_config_of_string :
  string -> web_hook_config
  (** Deserialize JSON data of type {!web_hook_config}. *)

val write_watch_action :
  Bi_outbuf.t -> watch_action -> unit
  (** Output a JSON value of type {!watch_action}. *)

val string_of_watch_action :
  ?len:int -> watch_action -> string
  (** Serialize a value of type {!watch_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_watch_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> watch_action
  (** Input JSON data of type {!watch_action}. *)

val watch_action_of_string :
  string -> watch_action
  (** Deserialize JSON data of type {!watch_action}. *)

val write_watch_event :
  Bi_outbuf.t -> watch_event -> unit
  (** Output a JSON value of type {!watch_event}. *)

val string_of_watch_event :
  ?len:int -> watch_event -> string
  (** Serialize a value of type {!watch_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_watch_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> watch_event
  (** Input JSON data of type {!watch_event}. *)

val watch_event_of_string :
  string -> watch_event
  (** Deserialize JSON data of type {!watch_event}. *)

val write_user_info :
  Bi_outbuf.t -> user_info -> unit
  (** Output a JSON value of type {!user_info}. *)

val string_of_user_info :
  ?len:int -> user_info -> string
  (** Serialize a value of type {!user_info}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_user_info :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> user_info
  (** Input JSON data of type {!user_info}. *)

val user_info_of_string :
  string -> user_info
  (** Deserialize JSON data of type {!user_info}. *)

val write_user :
  Bi_outbuf.t -> user -> unit
  (** Output a JSON value of type {!user}. *)

val string_of_user :
  ?len:int -> user -> string
  (** Serialize a value of type {!user}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_user :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> user
  (** Input JSON data of type {!user}. *)

val user_of_string :
  string -> user
  (** Deserialize JSON data of type {!user}. *)

val write_update_release :
  Bi_outbuf.t -> update_release -> unit
  (** Output a JSON value of type {!update_release}. *)

val string_of_update_release :
  ?len:int -> update_release -> string
  (** Serialize a value of type {!update_release}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_update_release :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> update_release
  (** Input JSON data of type {!update_release}. *)

val update_release_of_string :
  string -> update_release
  (** Deserialize JSON data of type {!update_release}. *)

val write_state :
  Bi_outbuf.t -> state -> unit
  (** Output a JSON value of type {!state}. *)

val string_of_state :
  ?len:int -> state -> string
  (** Serialize a value of type {!state}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_state :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> state
  (** Input JSON data of type {!state}. *)

val state_of_string :
  string -> state
  (** Deserialize JSON data of type {!state}. *)

val write_update_pull :
  Bi_outbuf.t -> update_pull -> unit
  (** Output a JSON value of type {!update_pull}. *)

val string_of_update_pull :
  ?len:int -> update_pull -> string
  (** Serialize a value of type {!update_pull}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_update_pull :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> update_pull
  (** Input JSON data of type {!update_pull}. *)

val update_pull_of_string :
  string -> update_pull
  (** Deserialize JSON data of type {!update_pull}. *)

val write_update_milestone :
  Bi_outbuf.t -> update_milestone -> unit
  (** Output a JSON value of type {!update_milestone}. *)

val string_of_update_milestone :
  ?len:int -> update_milestone -> string
  (** Serialize a value of type {!update_milestone}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_update_milestone :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> update_milestone
  (** Input JSON data of type {!update_milestone}. *)

val update_milestone_of_string :
  string -> update_milestone
  (** Deserialize JSON data of type {!update_milestone}. *)

val write_update_issue :
  Bi_outbuf.t -> update_issue -> unit
  (** Output a JSON value of type {!update_issue}. *)

val string_of_update_issue :
  ?len:int -> update_issue -> string
  (** Serialize a value of type {!update_issue}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_update_issue :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> update_issue
  (** Input JSON data of type {!update_issue}. *)

val update_issue_of_string :
  string -> update_issue
  (** Deserialize JSON data of type {!update_issue}. *)

val write_hook_config :
  Bi_outbuf.t -> hook_config -> unit
  (** Output a JSON value of type {!hook_config}. *)

val string_of_hook_config :
  ?len:int -> hook_config -> string
  (** Serialize a value of type {!hook_config}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_hook_config :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> hook_config
  (** Input JSON data of type {!hook_config}. *)

val hook_config_of_string :
  string -> hook_config
  (** Deserialize JSON data of type {!hook_config}. *)

val write_event_type :
  Bi_outbuf.t -> event_type -> unit
  (** Output a JSON value of type {!event_type}. *)

val string_of_event_type :
  ?len:int -> event_type -> string
  (** Serialize a value of type {!event_type}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_event_type :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> event_type
  (** Input JSON data of type {!event_type}. *)

val event_type_of_string :
  string -> event_type
  (** Deserialize JSON data of type {!event_type}. *)

val write_update_hook :
  Bi_outbuf.t -> update_hook -> unit
  (** Output a JSON value of type {!update_hook}. *)

val string_of_update_hook :
  ?len:int -> update_hook -> string
  (** Serialize a value of type {!update_hook}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_update_hook :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> update_hook
  (** Input JSON data of type {!update_hook}. *)

val update_hook_of_string :
  string -> update_hook
  (** Deserialize JSON data of type {!update_hook}. *)

val write_update_gist_file :
  Bi_outbuf.t -> update_gist_file -> unit
  (** Output a JSON value of type {!update_gist_file}. *)

val string_of_update_gist_file :
  ?len:int -> update_gist_file -> string
  (** Serialize a value of type {!update_gist_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_update_gist_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> update_gist_file
  (** Input JSON data of type {!update_gist_file}. *)

val update_gist_file_of_string :
  string -> update_gist_file
  (** Deserialize JSON data of type {!update_gist_file}. *)

val write_pull_ref :
  Bi_outbuf.t -> pull_ref -> unit
  (** Output a JSON value of type {!pull_ref}. *)

val string_of_pull_ref :
  ?len:int -> pull_ref -> string
  (** Serialize a value of type {!pull_ref}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pull_ref :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pull_ref
  (** Input JSON data of type {!pull_ref}. *)

val pull_ref_of_string :
  string -> pull_ref
  (** Deserialize JSON data of type {!pull_ref}. *)

val write_milestone :
  Bi_outbuf.t -> milestone -> unit
  (** Output a JSON value of type {!milestone}. *)

val string_of_milestone :
  ?len:int -> milestone -> string
  (** Serialize a value of type {!milestone}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_milestone :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> milestone
  (** Input JSON data of type {!milestone}. *)

val milestone_of_string :
  string -> milestone
  (** Deserialize JSON data of type {!milestone}. *)

val write_label :
  Bi_outbuf.t -> label -> unit
  (** Output a JSON value of type {!label}. *)

val string_of_label :
  ?len:int -> label -> string
  (** Serialize a value of type {!label}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_label :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> label
  (** Input JSON data of type {!label}. *)

val label_of_string :
  string -> label
  (** Deserialize JSON data of type {!label}. *)

val write_issue_sort :
  Bi_outbuf.t -> issue_sort -> unit
  (** Output a JSON value of type {!issue_sort}. *)

val string_of_issue_sort :
  ?len:int -> issue_sort -> string
  (** Serialize a value of type {!issue_sort}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_issue_sort :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> issue_sort
  (** Input JSON data of type {!issue_sort}. *)

val issue_sort_of_string :
  string -> issue_sort
  (** Deserialize JSON data of type {!issue_sort}. *)

val write_direction :
  Bi_outbuf.t -> direction -> unit
  (** Output a JSON value of type {!direction}. *)

val string_of_direction :
  ?len:int -> direction -> string
  (** Serialize a value of type {!direction}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_direction :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> direction
  (** Input JSON data of type {!direction}. *)

val direction_of_string :
  string -> direction
  (** Deserialize JSON data of type {!direction}. *)

val write_issue :
  Bi_outbuf.t -> issue -> unit
  (** Output a JSON value of type {!issue}. *)

val string_of_issue :
  ?len:int -> issue -> string
  (** Serialize a value of type {!issue}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_issue :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> issue
  (** Input JSON data of type {!issue}. *)

val issue_of_string :
  string -> issue
  (** Deserialize JSON data of type {!issue}. *)

val write_timeline_source :
  Bi_outbuf.t -> timeline_source -> unit
  (** Output a JSON value of type {!timeline_source}. *)

val string_of_timeline_source :
  ?len:int -> timeline_source -> string
  (** Serialize a value of type {!timeline_source}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_timeline_source :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> timeline_source
  (** Input JSON data of type {!timeline_source}. *)

val timeline_source_of_string :
  string -> timeline_source
  (** Deserialize JSON data of type {!timeline_source}. *)

val write_timeline_action :
  Bi_outbuf.t -> timeline_action -> unit
  (** Output a JSON value of type {!timeline_action}. *)

val string_of_timeline_action :
  ?len:int -> timeline_action -> string
  (** Serialize a value of type {!timeline_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_timeline_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> timeline_action
  (** Input JSON data of type {!timeline_action}. *)

val timeline_action_of_string :
  string -> timeline_action
  (** Deserialize JSON data of type {!timeline_action}. *)

val write_issue_rename :
  Bi_outbuf.t -> issue_rename -> unit
  (** Output a JSON value of type {!issue_rename}. *)

val string_of_issue_rename :
  ?len:int -> issue_rename -> string
  (** Serialize a value of type {!issue_rename}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_issue_rename :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> issue_rename
  (** Input JSON data of type {!issue_rename}. *)

val issue_rename_of_string :
  string -> issue_rename
  (** Deserialize JSON data of type {!issue_rename}. *)

val write_base_label :
  Bi_outbuf.t -> base_label -> unit
  (** Output a JSON value of type {!base_label}. *)

val string_of_base_label :
  ?len:int -> base_label -> string
  (** Serialize a value of type {!base_label}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_base_label :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> base_label
  (** Input JSON data of type {!base_label}. *)

val base_label_of_string :
  string -> base_label
  (** Deserialize JSON data of type {!base_label}. *)

val write_timeline_event :
  Bi_outbuf.t -> timeline_event -> unit
  (** Output a JSON value of type {!timeline_event}. *)

val string_of_timeline_event :
  ?len:int -> timeline_event -> string
  (** Serialize a value of type {!timeline_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_timeline_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> timeline_event
  (** Input JSON data of type {!timeline_event}. *)

val timeline_event_of_string :
  string -> timeline_event
  (** Deserialize JSON data of type {!timeline_event}. *)

val write_timeline_events :
  Bi_outbuf.t -> timeline_events -> unit
  (** Output a JSON value of type {!timeline_events}. *)

val string_of_timeline_events :
  ?len:int -> timeline_events -> string
  (** Serialize a value of type {!timeline_events}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_timeline_events :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> timeline_events
  (** Input JSON data of type {!timeline_events}. *)

val timeline_events_of_string :
  string -> timeline_events
  (** Deserialize JSON data of type {!timeline_events}. *)

val write_change :
  Bi_outbuf.t -> change -> unit
  (** Output a JSON value of type {!change}. *)

val string_of_change :
  ?len:int -> change -> string
  (** Serialize a value of type {!change}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_change :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> change
  (** Input JSON data of type {!change}. *)

val change_of_string :
  string -> change
  (** Deserialize JSON data of type {!change}. *)

val write_ticket_changes :
  Bi_outbuf.t -> ticket_changes -> unit
  (** Output a JSON value of type {!ticket_changes}. *)

val string_of_ticket_changes :
  ?len:int -> ticket_changes -> string
  (** Serialize a value of type {!ticket_changes}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ticket_changes :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ticket_changes
  (** Input JSON data of type {!ticket_changes}. *)

val ticket_changes_of_string :
  string -> ticket_changes
  (** Deserialize JSON data of type {!ticket_changes}. *)

val write_team :
  Bi_outbuf.t -> team -> unit
  (** Output a JSON value of type {!team}. *)

val string_of_team :
  ?len:int -> team -> string
  (** Serialize a value of type {!team}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_team :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> team
  (** Input JSON data of type {!team}. *)

val team_of_string :
  string -> team
  (** Deserialize JSON data of type {!team}. *)

val write_teams :
  Bi_outbuf.t -> teams -> unit
  (** Output a JSON value of type {!teams}. *)

val string_of_teams :
  ?len:int -> teams -> string
  (** Serialize a value of type {!teams}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_teams :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> teams
  (** Input JSON data of type {!teams}. *)

val teams_of_string :
  string -> teams
  (** Deserialize JSON data of type {!teams}. *)

val write_team_permission :
  Bi_outbuf.t -> team_permission -> unit
  (** Output a JSON value of type {!team_permission}. *)

val string_of_team_permission :
  ?len:int -> team_permission -> string
  (** Serialize a value of type {!team_permission}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_team_permission :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> team_permission
  (** Input JSON data of type {!team_permission}. *)

val team_permission_of_string :
  string -> team_permission
  (** Deserialize JSON data of type {!team_permission}. *)

val write_org :
  Bi_outbuf.t -> org -> unit
  (** Output a JSON value of type {!org}. *)

val string_of_org :
  ?len:int -> org -> string
  (** Serialize a value of type {!org}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_org :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> org
  (** Input JSON data of type {!org}. *)

val org_of_string :
  string -> org
  (** Deserialize JSON data of type {!org}. *)

val write_team_info :
  Bi_outbuf.t -> team_info -> unit
  (** Output a JSON value of type {!team_info}. *)

val string_of_team_info :
  ?len:int -> team_info -> string
  (** Serialize a value of type {!team_info}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_team_info :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> team_info
  (** Input JSON data of type {!team_info}. *)

val team_info_of_string :
  string -> team_info
  (** Deserialize JSON data of type {!team_info}. *)

val write_team_infos :
  Bi_outbuf.t -> team_infos -> unit
  (** Output a JSON value of type {!team_infos}. *)

val string_of_team_infos :
  ?len:int -> team_infos -> string
  (** Serialize a value of type {!team_infos}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_team_infos :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> team_infos
  (** Input JSON data of type {!team_infos}. *)

val team_infos_of_string :
  string -> team_infos
  (** Deserialize JSON data of type {!team_infos}. *)

val write_team_add_info :
  Bi_outbuf.t -> team_add_info -> unit
  (** Output a JSON value of type {!team_add_info}. *)

val string_of_team_add_info :
  ?len:int -> team_add_info -> string
  (** Serialize a value of type {!team_add_info}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_team_add_info :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> team_add_info
  (** Input JSON data of type {!team_add_info}. *)

val team_add_info_of_string :
  string -> team_add_info
  (** Deserialize JSON data of type {!team_add_info}. *)

val write_repository :
  Bi_outbuf.t -> repository -> unit
  (** Output a JSON value of type {!repository}. *)

val string_of_repository :
  ?len:int -> repository -> string
  (** Serialize a value of type {!repository}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repository :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repository
  (** Input JSON data of type {!repository}. *)

val repository_of_string :
  string -> repository
  (** Deserialize JSON data of type {!repository}. *)

val write_team_add_event :
  Bi_outbuf.t -> team_add_event -> unit
  (** Output a JSON value of type {!team_add_event}. *)

val string_of_team_add_event :
  ?len:int -> team_add_event -> string
  (** Serialize a value of type {!team_add_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_team_add_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> team_add_event
  (** Input JSON data of type {!team_add_event}. *)

val team_add_event_of_string :
  string -> team_add_event
  (** Deserialize JSON data of type {!team_add_event}. *)

val write_obj_type :
  Bi_outbuf.t -> obj_type -> unit
  (** Output a JSON value of type {!obj_type}. *)

val string_of_obj_type :
  ?len:int -> obj_type -> string
  (** Serialize a value of type {!obj_type}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_obj_type :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> obj_type
  (** Input JSON data of type {!obj_type}. *)

val obj_type_of_string :
  string -> obj_type
  (** Deserialize JSON data of type {!obj_type}. *)

val write_obj :
  Bi_outbuf.t -> obj -> unit
  (** Output a JSON value of type {!obj}. *)

val string_of_obj :
  ?len:int -> obj -> string
  (** Serialize a value of type {!obj}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_obj :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> obj
  (** Input JSON data of type {!obj}. *)

val obj_of_string :
  string -> obj
  (** Deserialize JSON data of type {!obj}. *)

val write_info :
  Bi_outbuf.t -> info -> unit
  (** Output a JSON value of type {!info}. *)

val string_of_info :
  ?len:int -> info -> string
  (** Serialize a value of type {!info}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_info :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> info
  (** Input JSON data of type {!info}. *)

val info_of_string :
  string -> info
  (** Deserialize JSON data of type {!info}. *)

val write_tag :
  Bi_outbuf.t -> tag -> unit
  (** Output a JSON value of type {!tag}. *)

val string_of_tag :
  ?len:int -> tag -> string
  (** Serialize a value of type {!tag}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tag :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tag
  (** Input JSON data of type {!tag}. *)

val tag_of_string :
  string -> tag
  (** Deserialize JSON data of type {!tag}. *)

val write_status_state :
  Bi_outbuf.t -> status_state -> unit
  (** Output a JSON value of type {!status_state}. *)

val string_of_status_state :
  ?len:int -> status_state -> string
  (** Serialize a value of type {!status_state}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_status_state :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> status_state
  (** Input JSON data of type {!status_state}. *)

val status_state_of_string :
  string -> status_state
  (** Deserialize JSON data of type {!status_state}. *)

val write_status :
  Bi_outbuf.t -> status -> unit
  (** Output a JSON value of type {!status}. *)

val string_of_status :
  ?len:int -> status -> string
  (** Serialize a value of type {!status}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_status :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> status
  (** Input JSON data of type {!status}. *)

val status_of_string :
  string -> status
  (** Deserialize JSON data of type {!status}. *)

val write_statuses :
  Bi_outbuf.t -> statuses -> unit
  (** Output a JSON value of type {!statuses}. *)

val string_of_statuses :
  ?len:int -> statuses -> string
  (** Serialize a value of type {!statuses}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_statuses :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> statuses
  (** Input JSON data of type {!statuses}. *)

val statuses_of_string :
  string -> statuses
  (** Deserialize JSON data of type {!statuses}. *)

val write_status_branch_commit :
  Bi_outbuf.t -> status_branch_commit -> unit
  (** Output a JSON value of type {!status_branch_commit}. *)

val string_of_status_branch_commit :
  ?len:int -> status_branch_commit -> string
  (** Serialize a value of type {!status_branch_commit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_status_branch_commit :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> status_branch_commit
  (** Input JSON data of type {!status_branch_commit}. *)

val status_branch_commit_of_string :
  string -> status_branch_commit
  (** Deserialize JSON data of type {!status_branch_commit}. *)

val write_status_branch :
  Bi_outbuf.t -> status_branch -> unit
  (** Output a JSON value of type {!status_branch}. *)

val string_of_status_branch :
  ?len:int -> status_branch -> string
  (** Serialize a value of type {!status_branch}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_status_branch :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> status_branch
  (** Input JSON data of type {!status_branch}. *)

val status_branch_of_string :
  string -> status_branch
  (** Deserialize JSON data of type {!status_branch}. *)

val write_git_commit :
  Bi_outbuf.t -> git_commit -> unit
  (** Output a JSON value of type {!git_commit}. *)

val string_of_git_commit :
  ?len:int -> git_commit -> string
  (** Serialize a value of type {!git_commit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_git_commit :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> git_commit
  (** Input JSON data of type {!git_commit}. *)

val git_commit_of_string :
  string -> git_commit
  (** Deserialize JSON data of type {!git_commit}. *)

val write_commit :
  Bi_outbuf.t -> commit -> unit
  (** Output a JSON value of type {!commit}. *)

val string_of_commit :
  ?len:int -> commit -> string
  (** Serialize a value of type {!commit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_commit :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> commit
  (** Input JSON data of type {!commit}. *)

val commit_of_string :
  string -> commit
  (** Deserialize JSON data of type {!commit}. *)

val write_status_event :
  Bi_outbuf.t -> status_event -> unit
  (** Output a JSON value of type {!status_event}. *)

val string_of_status_event :
  ?len:int -> status_event -> string
  (** Serialize a value of type {!status_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_status_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> status_event
  (** Input JSON data of type {!status_event}. *)

val status_event_of_string :
  string -> status_event
  (** Deserialize JSON data of type {!status_event}. *)

val write_scope :
  Bi_outbuf.t -> scope -> unit
  (** Output a JSON value of type {!scope}. *)

val string_of_scope :
  ?len:int -> scope -> string
  (** Serialize a value of type {!scope}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scope :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scope
  (** Input JSON data of type {!scope}. *)

val scope_of_string :
  string -> scope
  (** Deserialize JSON data of type {!scope}. *)

val write_repositories :
  Bi_outbuf.t -> repositories -> unit
  (** Output a JSON value of type {!repositories}. *)

val string_of_repositories :
  ?len:int -> repositories -> string
  (** Serialize a value of type {!repositories}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repositories :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repositories
  (** Input JSON data of type {!repositories}. *)

val repositories_of_string :
  string -> repositories
  (** Deserialize JSON data of type {!repositories}. *)

val write_repository_search :
  Bi_outbuf.t -> repository_search -> unit
  (** Output a JSON value of type {!repository_search}. *)

val string_of_repository_search :
  ?len:int -> repository_search -> string
  (** Serialize a value of type {!repository_search}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repository_search :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repository_search
  (** Input JSON data of type {!repository_search}. *)

val repository_search_of_string :
  string -> repository_search
  (** Deserialize JSON data of type {!repository_search}. *)

val write_repository_action :
  Bi_outbuf.t -> repository_action -> unit
  (** Output a JSON value of type {!repository_action}. *)

val string_of_repository_action :
  ?len:int -> repository_action -> string
  (** Serialize a value of type {!repository_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repository_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repository_action
  (** Input JSON data of type {!repository_action}. *)

val repository_action_of_string :
  string -> repository_action
  (** Deserialize JSON data of type {!repository_action}. *)

val write_repository_event :
  Bi_outbuf.t -> repository_event -> unit
  (** Output a JSON value of type {!repository_event}. *)

val string_of_repository_event :
  ?len:int -> repository_event -> string
  (** Serialize a value of type {!repository_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repository_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repository_event
  (** Input JSON data of type {!repository_event}. *)

val repository_event_of_string :
  string -> repository_event
  (** Deserialize JSON data of type {!repository_event}. *)

val write_repo_commit :
  Bi_outbuf.t -> repo_commit -> unit
  (** Output a JSON value of type {!repo_commit}. *)

val string_of_repo_commit :
  ?len:int -> repo_commit -> string
  (** Serialize a value of type {!repo_commit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repo_commit :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repo_commit
  (** Input JSON data of type {!repo_commit}. *)

val repo_commit_of_string :
  string -> repo_commit
  (** Deserialize JSON data of type {!repo_commit}. *)

val write_repo_tag :
  Bi_outbuf.t -> repo_tag -> unit
  (** Output a JSON value of type {!repo_tag}. *)

val string_of_repo_tag :
  ?len:int -> repo_tag -> string
  (** Serialize a value of type {!repo_tag}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repo_tag :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repo_tag
  (** Input JSON data of type {!repo_tag}. *)

val repo_tag_of_string :
  string -> repo_tag
  (** Deserialize JSON data of type {!repo_tag}. *)

val write_repo_tags :
  Bi_outbuf.t -> repo_tags -> unit
  (** Output a JSON value of type {!repo_tags}. *)

val string_of_repo_tags :
  ?len:int -> repo_tags -> string
  (** Serialize a value of type {!repo_tags}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repo_tags :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repo_tags
  (** Input JSON data of type {!repo_tags}. *)

val repo_tags_of_string :
  string -> repo_tags
  (** Deserialize JSON data of type {!repo_tags}. *)

val write_repo_issues_action :
  Bi_outbuf.t -> repo_issues_action -> unit
  (** Output a JSON value of type {!repo_issues_action}. *)

val string_of_repo_issues_action :
  ?len:int -> repo_issues_action -> string
  (** Serialize a value of type {!repo_issues_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repo_issues_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repo_issues_action
  (** Input JSON data of type {!repo_issues_action}. *)

val repo_issues_action_of_string :
  string -> repo_issues_action
  (** Deserialize JSON data of type {!repo_issues_action}. *)

val write_linked_user :
  Bi_outbuf.t -> linked_user -> unit
  (** Output a JSON value of type {!linked_user}. *)

val string_of_linked_user :
  ?len:int -> linked_user -> string
  (** Serialize a value of type {!linked_user}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_linked_user :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> linked_user
  (** Input JSON data of type {!linked_user}. *)

val linked_user_of_string :
  string -> linked_user
  (** Deserialize JSON data of type {!linked_user}. *)

val write_repo_issues_event :
  Bi_outbuf.t -> repo_issues_event -> unit
  (** Output a JSON value of type {!repo_issues_event}. *)

val string_of_repo_issues_event :
  ?len:int -> repo_issues_event -> string
  (** Serialize a value of type {!repo_issues_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repo_issues_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repo_issues_event
  (** Input JSON data of type {!repo_issues_event}. *)

val repo_issues_event_of_string :
  string -> repo_issues_event
  (** Deserialize JSON data of type {!repo_issues_event}. *)

val write_repo_issues_events :
  Bi_outbuf.t -> repo_issues_events -> unit
  (** Output a JSON value of type {!repo_issues_events}. *)

val string_of_repo_issues_events :
  ?len:int -> repo_issues_events -> string
  (** Serialize a value of type {!repo_issues_events}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repo_issues_events :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repo_issues_events
  (** Input JSON data of type {!repo_issues_events}. *)

val repo_issues_events_of_string :
  string -> repo_issues_events
  (** Deserialize JSON data of type {!repo_issues_events}. *)

val write_repo_issue_event :
  Bi_outbuf.t -> repo_issue_event -> unit
  (** Output a JSON value of type {!repo_issue_event}. *)

val string_of_repo_issue_event :
  ?len:int -> repo_issue_event -> string
  (** Serialize a value of type {!repo_issue_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repo_issue_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repo_issue_event
  (** Input JSON data of type {!repo_issue_event}. *)

val repo_issue_event_of_string :
  string -> repo_issue_event
  (** Deserialize JSON data of type {!repo_issue_event}. *)

val write_repo_issue_events :
  Bi_outbuf.t -> repo_issue_events -> unit
  (** Output a JSON value of type {!repo_issue_events}. *)

val string_of_repo_issue_events :
  ?len:int -> repo_issue_events -> string
  (** Serialize a value of type {!repo_issue_events}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repo_issue_events :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repo_issue_events
  (** Input JSON data of type {!repo_issue_events}. *)

val repo_issue_events_of_string :
  string -> repo_issue_events
  (** Deserialize JSON data of type {!repo_issue_events}. *)

val write_repo_branch :
  Bi_outbuf.t -> repo_branch -> unit
  (** Output a JSON value of type {!repo_branch}. *)

val string_of_repo_branch :
  ?len:int -> repo_branch -> string
  (** Serialize a value of type {!repo_branch}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repo_branch :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repo_branch
  (** Input JSON data of type {!repo_branch}. *)

val repo_branch_of_string :
  string -> repo_branch
  (** Deserialize JSON data of type {!repo_branch}. *)

val write_repo_branches :
  Bi_outbuf.t -> repo_branches -> unit
  (** Output a JSON value of type {!repo_branches}. *)

val string_of_repo_branches :
  ?len:int -> repo_branches -> string
  (** Serialize a value of type {!repo_branches}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repo_branches :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repo_branches
  (** Input JSON data of type {!repo_branches}. *)

val repo_branches_of_string :
  string -> repo_branches
  (** Deserialize JSON data of type {!repo_branches}. *)

val write_repo :
  Bi_outbuf.t -> repo -> unit
  (** Output a JSON value of type {!repo}. *)

val string_of_repo :
  ?len:int -> repo -> string
  (** Serialize a value of type {!repo}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_repo :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> repo
  (** Input JSON data of type {!repo}. *)

val repo_of_string :
  string -> repo
  (** Deserialize JSON data of type {!repo}. *)

val write_release :
  Bi_outbuf.t -> release -> unit
  (** Output a JSON value of type {!release}. *)

val string_of_release :
  ?len:int -> release -> string
  (** Serialize a value of type {!release}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_release :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> release
  (** Input JSON data of type {!release}. *)

val release_of_string :
  string -> release
  (** Deserialize JSON data of type {!release}. *)

val write_releases :
  Bi_outbuf.t -> releases -> unit
  (** Output a JSON value of type {!releases}. *)

val string_of_releases :
  ?len:int -> releases -> string
  (** Serialize a value of type {!releases}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_releases :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> releases
  (** Input JSON data of type {!releases}. *)

val releases_of_string :
  string -> releases
  (** Deserialize JSON data of type {!releases}. *)

val write_release_repo :
  Bi_outbuf.t -> release_repo -> unit
  (** Output a JSON value of type {!release_repo}. *)

val string_of_release_repo :
  ?len:int -> release_repo -> string
  (** Serialize a value of type {!release_repo}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_release_repo :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> release_repo
  (** Input JSON data of type {!release_repo}. *)

val release_repo_of_string :
  string -> release_repo
  (** Deserialize JSON data of type {!release_repo}. *)

val write_release_repos :
  Bi_outbuf.t -> release_repos -> unit
  (** Output a JSON value of type {!release_repos}. *)

val string_of_release_repos :
  ?len:int -> release_repos -> string
  (** Serialize a value of type {!release_repos}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_release_repos :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> release_repos
  (** Input JSON data of type {!release_repos}. *)

val release_repos_of_string :
  string -> release_repos
  (** Deserialize JSON data of type {!release_repos}. *)

val write_release_action :
  Bi_outbuf.t -> release_action -> unit
  (** Output a JSON value of type {!release_action}. *)

val string_of_release_action :
  ?len:int -> release_action -> string
  (** Serialize a value of type {!release_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_release_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> release_action
  (** Input JSON data of type {!release_action}. *)

val release_action_of_string :
  string -> release_action
  (** Deserialize JSON data of type {!release_action}. *)

val write_release_event :
  Bi_outbuf.t -> release_event -> unit
  (** Output a JSON value of type {!release_event}. *)

val string_of_release_event :
  ?len:int -> release_event -> string
  (** Serialize a value of type {!release_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_release_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> release_event
  (** Input JSON data of type {!release_event}. *)

val release_event_of_string :
  string -> release_event
  (** Deserialize JSON data of type {!release_event}. *)

val write_ref :
  Bi_outbuf.t -> ref -> unit
  (** Output a JSON value of type {!ref}. *)

val string_of_ref :
  ?len:int -> ref -> string
  (** Serialize a value of type {!ref}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ref :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ref
  (** Input JSON data of type {!ref}. *)

val ref_of_string :
  string -> ref
  (** Deserialize JSON data of type {!ref}. *)

val write_rate :
  Bi_outbuf.t -> rate -> unit
  (** Output a JSON value of type {!rate}. *)

val string_of_rate :
  ?len:int -> rate -> string
  (** Serialize a value of type {!rate}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_rate :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> rate
  (** Input JSON data of type {!rate}. *)

val rate_of_string :
  string -> rate
  (** Deserialize JSON data of type {!rate}. *)

val write_rate_resources :
  Bi_outbuf.t -> rate_resources -> unit
  (** Output a JSON value of type {!rate_resources}. *)

val string_of_rate_resources :
  ?len:int -> rate_resources -> string
  (** Serialize a value of type {!rate_resources}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_rate_resources :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> rate_resources
  (** Input JSON data of type {!rate_resources}. *)

val rate_resources_of_string :
  string -> rate_resources
  (** Deserialize JSON data of type {!rate_resources}. *)

val write_rate_limit :
  Bi_outbuf.t -> rate_limit -> unit
  (** Output a JSON value of type {!rate_limit}. *)

val string_of_rate_limit :
  ?len:int -> rate_limit -> string
  (** Serialize a value of type {!rate_limit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_rate_limit :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> rate_limit
  (** Input JSON data of type {!rate_limit}. *)

val rate_limit_of_string :
  string -> rate_limit
  (** Deserialize JSON data of type {!rate_limit}. *)

val write_push_event_author :
  Bi_outbuf.t -> push_event_author -> unit
  (** Output a JSON value of type {!push_event_author}. *)

val string_of_push_event_author :
  ?len:int -> push_event_author -> string
  (** Serialize a value of type {!push_event_author}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_push_event_author :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> push_event_author
  (** Input JSON data of type {!push_event_author}. *)

val push_event_author_of_string :
  string -> push_event_author
  (** Deserialize JSON data of type {!push_event_author}. *)

val write_push_event_hook_commit :
  Bi_outbuf.t -> push_event_hook_commit -> unit
  (** Output a JSON value of type {!push_event_hook_commit}. *)

val string_of_push_event_hook_commit :
  ?len:int -> push_event_hook_commit -> string
  (** Serialize a value of type {!push_event_hook_commit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_push_event_hook_commit :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> push_event_hook_commit
  (** Input JSON data of type {!push_event_hook_commit}. *)

val push_event_hook_commit_of_string :
  string -> push_event_hook_commit
  (** Deserialize JSON data of type {!push_event_hook_commit}. *)

val write_push_event_hook :
  Bi_outbuf.t -> push_event_hook -> unit
  (** Output a JSON value of type {!push_event_hook}. *)

val string_of_push_event_hook :
  ?len:int -> push_event_hook -> string
  (** Serialize a value of type {!push_event_hook}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_push_event_hook :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> push_event_hook
  (** Input JSON data of type {!push_event_hook}. *)

val push_event_hook_of_string :
  string -> push_event_hook
  (** Deserialize JSON data of type {!push_event_hook}. *)

val write_push_event_commit_base :
  Bi_outbuf.t -> push_event_commit_base -> unit
  (** Output a JSON value of type {!push_event_commit_base}. *)

val string_of_push_event_commit_base :
  ?len:int -> push_event_commit_base -> string
  (** Serialize a value of type {!push_event_commit_base}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_push_event_commit_base :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> push_event_commit_base
  (** Input JSON data of type {!push_event_commit_base}. *)

val push_event_commit_base_of_string :
  string -> push_event_commit_base
  (** Deserialize JSON data of type {!push_event_commit_base}. *)

val write_push_event_commit :
  Bi_outbuf.t -> push_event_commit -> unit
  (** Output a JSON value of type {!push_event_commit}. *)

val string_of_push_event_commit :
  ?len:int -> push_event_commit -> string
  (** Serialize a value of type {!push_event_commit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_push_event_commit :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> push_event_commit
  (** Input JSON data of type {!push_event_commit}. *)

val push_event_commit_of_string :
  string -> push_event_commit
  (** Deserialize JSON data of type {!push_event_commit}. *)

val write_push_event_base :
  Bi_outbuf.t -> push_event_base -> unit
  (** Output a JSON value of type {!push_event_base}. *)

val string_of_push_event_base :
  ?len:int -> push_event_base -> string
  (** Serialize a value of type {!push_event_base}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_push_event_base :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> push_event_base
  (** Input JSON data of type {!push_event_base}. *)

val push_event_base_of_string :
  string -> push_event_base
  (** Deserialize JSON data of type {!push_event_base}. *)

val write_push_event :
  Bi_outbuf.t -> push_event -> unit
  (** Output a JSON value of type {!push_event}. *)

val string_of_push_event :
  ?len:int -> push_event -> string
  (** Serialize a value of type {!push_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_push_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> push_event
  (** Input JSON data of type {!push_event}. *)

val push_event_of_string :
  string -> push_event
  (** Deserialize JSON data of type {!push_event}. *)

val write_link :
  Bi_outbuf.t -> link -> unit
  (** Output a JSON value of type {!link}. *)

val string_of_link :
  ?len:int -> link -> string
  (** Serialize a value of type {!link}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_link :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> link
  (** Input JSON data of type {!link}. *)

val link_of_string :
  string -> link
  (** Deserialize JSON data of type {!link}. *)

val write_pull_links :
  Bi_outbuf.t -> pull_links -> unit
  (** Output a JSON value of type {!pull_links}. *)

val string_of_pull_links :
  ?len:int -> pull_links -> string
  (** Serialize a value of type {!pull_links}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pull_links :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pull_links
  (** Input JSON data of type {!pull_links}. *)

val pull_links_of_string :
  string -> pull_links
  (** Deserialize JSON data of type {!pull_links}. *)

val write_branch :
  Bi_outbuf.t -> branch -> unit
  (** Output a JSON value of type {!branch}. *)

val string_of_branch :
  ?len:int -> branch -> string
  (** Serialize a value of type {!branch}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_branch :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> branch
  (** Input JSON data of type {!branch}. *)

val branch_of_string :
  string -> branch
  (** Deserialize JSON data of type {!branch}. *)

val write_pull :
  Bi_outbuf.t -> pull -> unit
  (** Output a JSON value of type {!pull}. *)

val string_of_pull :
  ?len:int -> pull -> string
  (** Serialize a value of type {!pull}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pull :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pull
  (** Input JSON data of type {!pull}. *)

val pull_of_string :
  string -> pull
  (** Deserialize JSON data of type {!pull}. *)

val write_pulls :
  Bi_outbuf.t -> pulls -> unit
  (** Output a JSON value of type {!pulls}. *)

val string_of_pulls :
  ?len:int -> pulls -> string
  (** Serialize a value of type {!pulls}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pulls :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pulls
  (** Input JSON data of type {!pulls}. *)

val pulls_of_string :
  string -> pulls
  (** Deserialize JSON data of type {!pulls}. *)

val write_body_changes :
  Bi_outbuf.t -> body_changes -> unit
  (** Output a JSON value of type {!body_changes}. *)

val string_of_body_changes :
  ?len:int -> body_changes -> string
  (** Serialize a value of type {!body_changes}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_body_changes :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> body_changes
  (** Input JSON data of type {!body_changes}. *)

val body_changes_of_string :
  string -> body_changes
  (** Deserialize JSON data of type {!body_changes}. *)

val write_pull_request_review_comment_action :
  Bi_outbuf.t -> pull_request_review_comment_action -> unit
  (** Output a JSON value of type {!pull_request_review_comment_action}. *)

val string_of_pull_request_review_comment_action :
  ?len:int -> pull_request_review_comment_action -> string
  (** Serialize a value of type {!pull_request_review_comment_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pull_request_review_comment_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pull_request_review_comment_action
  (** Input JSON data of type {!pull_request_review_comment_action}. *)

val pull_request_review_comment_action_of_string :
  string -> pull_request_review_comment_action
  (** Deserialize JSON data of type {!pull_request_review_comment_action}. *)

val write_pull_request_review_comment :
  Bi_outbuf.t -> pull_request_review_comment -> unit
  (** Output a JSON value of type {!pull_request_review_comment}. *)

val string_of_pull_request_review_comment :
  ?len:int -> pull_request_review_comment -> string
  (** Serialize a value of type {!pull_request_review_comment}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pull_request_review_comment :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pull_request_review_comment
  (** Input JSON data of type {!pull_request_review_comment}. *)

val pull_request_review_comment_of_string :
  string -> pull_request_review_comment
  (** Deserialize JSON data of type {!pull_request_review_comment}. *)

val write_pull_request_review_comment_event :
  Bi_outbuf.t -> pull_request_review_comment_event -> unit
  (** Output a JSON value of type {!pull_request_review_comment_event}. *)

val string_of_pull_request_review_comment_event :
  ?len:int -> pull_request_review_comment_event -> string
  (** Serialize a value of type {!pull_request_review_comment_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pull_request_review_comment_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pull_request_review_comment_event
  (** Input JSON data of type {!pull_request_review_comment_event}. *)

val pull_request_review_comment_event_of_string :
  string -> pull_request_review_comment_event
  (** Deserialize JSON data of type {!pull_request_review_comment_event}. *)

val write_pull_request_action :
  Bi_outbuf.t -> pull_request_action -> unit
  (** Output a JSON value of type {!pull_request_action}. *)

val string_of_pull_request_action :
  ?len:int -> pull_request_action -> string
  (** Serialize a value of type {!pull_request_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pull_request_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pull_request_action
  (** Input JSON data of type {!pull_request_action}. *)

val pull_request_action_of_string :
  string -> pull_request_action
  (** Deserialize JSON data of type {!pull_request_action}. *)

val write_pull_request_event :
  Bi_outbuf.t -> pull_request_event -> unit
  (** Output a JSON value of type {!pull_request_event}. *)

val string_of_pull_request_event :
  ?len:int -> pull_request_event -> string
  (** Serialize a value of type {!pull_request_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pull_request_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pull_request_event
  (** Input JSON data of type {!pull_request_event}. *)

val pull_request_event_of_string :
  string -> pull_request_event
  (** Deserialize JSON data of type {!pull_request_event}. *)

val write_page_build_status :
  Bi_outbuf.t -> page_build_status -> unit
  (** Output a JSON value of type {!page_build_status}. *)

val string_of_page_build_status :
  ?len:int -> page_build_status -> string
  (** Serialize a value of type {!page_build_status}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_page_build_status :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> page_build_status
  (** Input JSON data of type {!page_build_status}. *)

val page_build_status_of_string :
  string -> page_build_status
  (** Deserialize JSON data of type {!page_build_status}. *)

val write_page_build_error :
  Bi_outbuf.t -> page_build_error -> unit
  (** Output a JSON value of type {!page_build_error}. *)

val string_of_page_build_error :
  ?len:int -> page_build_error -> string
  (** Serialize a value of type {!page_build_error}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_page_build_error :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> page_build_error
  (** Input JSON data of type {!page_build_error}. *)

val page_build_error_of_string :
  string -> page_build_error
  (** Deserialize JSON data of type {!page_build_error}. *)

val write_page_build :
  Bi_outbuf.t -> page_build -> unit
  (** Output a JSON value of type {!page_build}. *)

val string_of_page_build :
  ?len:int -> page_build -> string
  (** Serialize a value of type {!page_build}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_page_build :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> page_build
  (** Input JSON data of type {!page_build}. *)

val page_build_of_string :
  string -> page_build
  (** Deserialize JSON data of type {!page_build}. *)

val write_page_build_event :
  Bi_outbuf.t -> page_build_event -> unit
  (** Output a JSON value of type {!page_build_event}. *)

val string_of_page_build_event :
  ?len:int -> page_build_event -> string
  (** Serialize a value of type {!page_build_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_page_build_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> page_build_event
  (** Input JSON data of type {!page_build_event}. *)

val page_build_event_of_string :
  string -> page_build_event
  (** Deserialize JSON data of type {!page_build_event}. *)

val write_orgs :
  Bi_outbuf.t -> orgs -> unit
  (** Output a JSON value of type {!orgs}. *)

val string_of_orgs :
  ?len:int -> orgs -> string
  (** Serialize a value of type {!orgs}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_orgs :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> orgs
  (** Input JSON data of type {!orgs}. *)

val orgs_of_string :
  string -> orgs
  (** Deserialize JSON data of type {!orgs}. *)

val write_organization :
  Bi_outbuf.t -> organization -> unit
  (** Output a JSON value of type {!organization}. *)

val string_of_organization :
  ?len:int -> organization -> string
  (** Serialize a value of type {!organization}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_organization :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> organization
  (** Input JSON data of type {!organization}. *)

val organization_of_string :
  string -> organization
  (** Deserialize JSON data of type {!organization}. *)

val write_new_status :
  Bi_outbuf.t -> new_status -> unit
  (** Output a JSON value of type {!new_status}. *)

val string_of_new_status :
  ?len:int -> new_status -> string
  (** Serialize a value of type {!new_status}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_status :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_status
  (** Input JSON data of type {!new_status}. *)

val new_status_of_string :
  string -> new_status
  (** Deserialize JSON data of type {!new_status}. *)

val write_new_repo :
  Bi_outbuf.t -> new_repo -> unit
  (** Output a JSON value of type {!new_repo}. *)

val string_of_new_repo :
  ?len:int -> new_repo -> string
  (** Serialize a value of type {!new_repo}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_repo :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_repo
  (** Input JSON data of type {!new_repo}. *)

val new_repo_of_string :
  string -> new_repo
  (** Deserialize JSON data of type {!new_repo}. *)

val write_new_release :
  Bi_outbuf.t -> new_release -> unit
  (** Output a JSON value of type {!new_release}. *)

val string_of_new_release :
  ?len:int -> new_release -> string
  (** Serialize a value of type {!new_release}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_release :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_release
  (** Input JSON data of type {!new_release}. *)

val new_release_of_string :
  string -> new_release
  (** Deserialize JSON data of type {!new_release}. *)

val write_new_pull_issue :
  Bi_outbuf.t -> new_pull_issue -> unit
  (** Output a JSON value of type {!new_pull_issue}. *)

val string_of_new_pull_issue :
  ?len:int -> new_pull_issue -> string
  (** Serialize a value of type {!new_pull_issue}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_pull_issue :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_pull_issue
  (** Input JSON data of type {!new_pull_issue}. *)

val new_pull_issue_of_string :
  string -> new_pull_issue
  (** Deserialize JSON data of type {!new_pull_issue}. *)

val write_new_pull :
  Bi_outbuf.t -> new_pull -> unit
  (** Output a JSON value of type {!new_pull}. *)

val string_of_new_pull :
  ?len:int -> new_pull -> string
  (** Serialize a value of type {!new_pull}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_pull :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_pull
  (** Input JSON data of type {!new_pull}. *)

val new_pull_of_string :
  string -> new_pull
  (** Deserialize JSON data of type {!new_pull}. *)

val write_new_milestone :
  Bi_outbuf.t -> new_milestone -> unit
  (** Output a JSON value of type {!new_milestone}. *)

val string_of_new_milestone :
  ?len:int -> new_milestone -> string
  (** Serialize a value of type {!new_milestone}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_milestone :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_milestone
  (** Input JSON data of type {!new_milestone}. *)

val new_milestone_of_string :
  string -> new_milestone
  (** Deserialize JSON data of type {!new_milestone}. *)

val write_new_label :
  Bi_outbuf.t -> new_label -> unit
  (** Output a JSON value of type {!new_label}. *)

val string_of_new_label :
  ?len:int -> new_label -> string
  (** Serialize a value of type {!new_label}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_label :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_label
  (** Input JSON data of type {!new_label}. *)

val new_label_of_string :
  string -> new_label
  (** Deserialize JSON data of type {!new_label}. *)

val write_new_issue_comment :
  Bi_outbuf.t -> new_issue_comment -> unit
  (** Output a JSON value of type {!new_issue_comment}. *)

val string_of_new_issue_comment :
  ?len:int -> new_issue_comment -> string
  (** Serialize a value of type {!new_issue_comment}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_issue_comment :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_issue_comment
  (** Input JSON data of type {!new_issue_comment}. *)

val new_issue_comment_of_string :
  string -> new_issue_comment
  (** Deserialize JSON data of type {!new_issue_comment}. *)

val write_new_issue :
  Bi_outbuf.t -> new_issue -> unit
  (** Output a JSON value of type {!new_issue}. *)

val string_of_new_issue :
  ?len:int -> new_issue -> string
  (** Serialize a value of type {!new_issue}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_issue :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_issue
  (** Input JSON data of type {!new_issue}. *)

val new_issue_of_string :
  string -> new_issue
  (** Deserialize JSON data of type {!new_issue}. *)

val write_new_hook :
  Bi_outbuf.t -> new_hook -> unit
  (** Output a JSON value of type {!new_hook}. *)

val string_of_new_hook :
  ?len:int -> new_hook -> string
  (** Serialize a value of type {!new_hook}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_hook :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_hook
  (** Input JSON data of type {!new_hook}. *)

val new_hook_of_string :
  string -> new_hook
  (** Deserialize JSON data of type {!new_hook}. *)

val write_new_gist_content :
  Bi_outbuf.t -> new_gist_content -> unit
  (** Output a JSON value of type {!new_gist_content}. *)

val string_of_new_gist_content :
  ?len:int -> new_gist_content -> string
  (** Serialize a value of type {!new_gist_content}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_gist_content :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_gist_content
  (** Input JSON data of type {!new_gist_content}. *)

val new_gist_content_of_string :
  string -> new_gist_content
  (** Deserialize JSON data of type {!new_gist_content}. *)

val write_new_gist_contents :
  Bi_outbuf.t -> new_gist_contents -> unit
  (** Output a JSON value of type {!new_gist_contents}. *)

val string_of_new_gist_contents :
  ?len:int -> new_gist_contents -> string
  (** Serialize a value of type {!new_gist_contents}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_gist_contents :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_gist_contents
  (** Input JSON data of type {!new_gist_contents}. *)

val new_gist_contents_of_string :
  string -> new_gist_contents
  (** Deserialize JSON data of type {!new_gist_contents}. *)

val write_new_gist :
  Bi_outbuf.t -> new_gist -> unit
  (** Output a JSON value of type {!new_gist}. *)

val string_of_new_gist :
  ?len:int -> new_gist -> string
  (** Serialize a value of type {!new_gist}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_gist :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_gist
  (** Input JSON data of type {!new_gist}. *)

val new_gist_of_string :
  string -> new_gist
  (** Deserialize JSON data of type {!new_gist}. *)

val write_new_deploy_key :
  Bi_outbuf.t -> new_deploy_key -> unit
  (** Output a JSON value of type {!new_deploy_key}. *)

val string_of_new_deploy_key :
  ?len:int -> new_deploy_key -> string
  (** Serialize a value of type {!new_deploy_key}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_new_deploy_key :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> new_deploy_key
  (** Input JSON data of type {!new_deploy_key}. *)

val new_deploy_key_of_string :
  string -> new_deploy_key
  (** Deserialize JSON data of type {!new_deploy_key}. *)

val write_milestones :
  Bi_outbuf.t -> milestones -> unit
  (** Output a JSON value of type {!milestones}. *)

val string_of_milestones :
  ?len:int -> milestones -> string
  (** Serialize a value of type {!milestones}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_milestones :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> milestones
  (** Input JSON data of type {!milestones}. *)

val milestones_of_string :
  string -> milestones
  (** Deserialize JSON data of type {!milestones}. *)

val write_milestone_sort :
  Bi_outbuf.t -> milestone_sort -> unit
  (** Output a JSON value of type {!milestone_sort}. *)

val string_of_milestone_sort :
  ?len:int -> milestone_sort -> string
  (** Serialize a value of type {!milestone_sort}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_milestone_sort :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> milestone_sort
  (** Input JSON data of type {!milestone_sort}. *)

val milestone_sort_of_string :
  string -> milestone_sort
  (** Deserialize JSON data of type {!milestone_sort}. *)

val write_error :
  Bi_outbuf.t -> error -> unit
  (** Output a JSON value of type {!error}. *)

val string_of_error :
  ?len:int -> error -> string
  (** Serialize a value of type {!error}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_error :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> error
  (** Input JSON data of type {!error}. *)

val error_of_string :
  string -> error
  (** Deserialize JSON data of type {!error}. *)

val write_message :
  Bi_outbuf.t -> message -> unit
  (** Output a JSON value of type {!message}. *)

val string_of_message :
  ?len:int -> message -> string
  (** Serialize a value of type {!message}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_message :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> message
  (** Input JSON data of type {!message}. *)

val message_of_string :
  string -> message
  (** Deserialize JSON data of type {!message}. *)

val write_merge_request :
  Bi_outbuf.t -> merge_request -> unit
  (** Output a JSON value of type {!merge_request}. *)

val string_of_merge_request :
  ?len:int -> merge_request -> string
  (** Serialize a value of type {!merge_request}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_merge_request :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> merge_request
  (** Input JSON data of type {!merge_request}. *)

val merge_request_of_string :
  string -> merge_request
  (** Deserialize JSON data of type {!merge_request}. *)

val write_merge :
  Bi_outbuf.t -> merge -> unit
  (** Output a JSON value of type {!merge}. *)

val string_of_merge :
  ?len:int -> merge -> string
  (** Serialize a value of type {!merge}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_merge :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> merge
  (** Input JSON data of type {!merge}. *)

val merge_of_string :
  string -> merge
  (** Deserialize JSON data of type {!merge}. *)

val write_member_action :
  Bi_outbuf.t -> member_action -> unit
  (** Output a JSON value of type {!member_action}. *)

val string_of_member_action :
  ?len:int -> member_action -> string
  (** Serialize a value of type {!member_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_member_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> member_action
  (** Input JSON data of type {!member_action}. *)

val member_action_of_string :
  string -> member_action
  (** Deserialize JSON data of type {!member_action}. *)

val write_member_event :
  Bi_outbuf.t -> member_event -> unit
  (** Output a JSON value of type {!member_event}. *)

val string_of_member_event :
  ?len:int -> member_event -> string
  (** Serialize a value of type {!member_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_member_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> member_event
  (** Input JSON data of type {!member_event}. *)

val member_event_of_string :
  string -> member_event
  (** Deserialize JSON data of type {!member_event}. *)

val write_linked_users :
  Bi_outbuf.t -> linked_users -> unit
  (** Output a JSON value of type {!linked_users}. *)

val string_of_linked_users :
  ?len:int -> linked_users -> string
  (** Serialize a value of type {!linked_users}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_linked_users :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> linked_users
  (** Input JSON data of type {!linked_users}. *)

val linked_users_of_string :
  string -> linked_users
  (** Deserialize JSON data of type {!linked_users}. *)

val write_labels :
  Bi_outbuf.t -> labels -> unit
  (** Output a JSON value of type {!labels}. *)

val string_of_labels :
  ?len:int -> labels -> string
  (** Serialize a value of type {!labels}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_labels :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> labels
  (** Input JSON data of type {!labels}. *)

val labels_of_string :
  string -> labels
  (** Deserialize JSON data of type {!labels}. *)

val write_label_names :
  Bi_outbuf.t -> label_names -> unit
  (** Output a JSON value of type {!label_names}. *)

val string_of_label_names :
  ?len:int -> label_names -> string
  (** Serialize a value of type {!label_names}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_label_names :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> label_names
  (** Input JSON data of type {!label_names}. *)

val label_names_of_string :
  string -> label_names
  (** Deserialize JSON data of type {!label_names}. *)

val write_issues_action :
  Bi_outbuf.t -> issues_action -> unit
  (** Output a JSON value of type {!issues_action}. *)

val string_of_issues_action :
  ?len:int -> issues_action -> string
  (** Serialize a value of type {!issues_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_issues_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> issues_action
  (** Input JSON data of type {!issues_action}. *)

val issues_action_of_string :
  string -> issues_action
  (** Deserialize JSON data of type {!issues_action}. *)

val write_issues_event :
  Bi_outbuf.t -> issues_event -> unit
  (** Output a JSON value of type {!issues_event}. *)

val string_of_issues_event :
  ?len:int -> issues_event -> string
  (** Serialize a value of type {!issues_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_issues_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> issues_event
  (** Input JSON data of type {!issues_event}. *)

val issues_event_of_string :
  string -> issues_event
  (** Deserialize JSON data of type {!issues_event}. *)

val write_issues :
  Bi_outbuf.t -> issues -> unit
  (** Output a JSON value of type {!issues}. *)

val string_of_issues :
  ?len:int -> issues -> string
  (** Serialize a value of type {!issues}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_issues :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> issues
  (** Input JSON data of type {!issues}. *)

val issues_of_string :
  string -> issues
  (** Deserialize JSON data of type {!issues}. *)

val write_issue_comment :
  Bi_outbuf.t -> issue_comment -> unit
  (** Output a JSON value of type {!issue_comment}. *)

val string_of_issue_comment :
  ?len:int -> issue_comment -> string
  (** Serialize a value of type {!issue_comment}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_issue_comment :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> issue_comment
  (** Input JSON data of type {!issue_comment}. *)

val issue_comment_of_string :
  string -> issue_comment
  (** Deserialize JSON data of type {!issue_comment}. *)

val write_issue_comments :
  Bi_outbuf.t -> issue_comments -> unit
  (** Output a JSON value of type {!issue_comments}. *)

val string_of_issue_comments :
  ?len:int -> issue_comments -> string
  (** Serialize a value of type {!issue_comments}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_issue_comments :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> issue_comments
  (** Input JSON data of type {!issue_comments}. *)

val issue_comments_of_string :
  string -> issue_comments
  (** Deserialize JSON data of type {!issue_comments}. *)

val write_issue_comment_action :
  Bi_outbuf.t -> issue_comment_action -> unit
  (** Output a JSON value of type {!issue_comment_action}. *)

val string_of_issue_comment_action :
  ?len:int -> issue_comment_action -> string
  (** Serialize a value of type {!issue_comment_action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_issue_comment_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> issue_comment_action
  (** Input JSON data of type {!issue_comment_action}. *)

val issue_comment_action_of_string :
  string -> issue_comment_action
  (** Deserialize JSON data of type {!issue_comment_action}. *)

val write_issue_comment_event :
  Bi_outbuf.t -> issue_comment_event -> unit
  (** Output a JSON value of type {!issue_comment_event}. *)

val string_of_issue_comment_event :
  ?len:int -> issue_comment_event -> string
  (** Serialize a value of type {!issue_comment_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_issue_comment_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> issue_comment_event
  (** Input JSON data of type {!issue_comment_event}. *)

val issue_comment_event_of_string :
  string -> issue_comment_event
  (** Deserialize JSON data of type {!issue_comment_event}. *)

val write_hook :
  Bi_outbuf.t -> hook -> unit
  (** Output a JSON value of type {!hook}. *)

val string_of_hook :
  ?len:int -> hook -> string
  (** Serialize a value of type {!hook}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_hook :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> hook
  (** Input JSON data of type {!hook}. *)

val hook_of_string :
  string -> hook
  (** Deserialize JSON data of type {!hook}. *)

val write_hooks :
  Bi_outbuf.t -> hooks -> unit
  (** Output a JSON value of type {!hooks}. *)

val string_of_hooks :
  ?len:int -> hooks -> string
  (** Serialize a value of type {!hooks}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_hooks :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> hooks
  (** Input JSON data of type {!hooks}. *)

val hooks_of_string :
  string -> hooks
  (** Deserialize JSON data of type {!hooks}. *)

val write_gollum_event :
  Bi_outbuf.t -> gollum_event -> unit
  (** Output a JSON value of type {!gollum_event}. *)

val string_of_gollum_event :
  ?len:int -> gollum_event -> string
  (** Serialize a value of type {!gollum_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_gollum_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> gollum_event
  (** Input JSON data of type {!gollum_event}. *)

val gollum_event_of_string :
  string -> gollum_event
  (** Deserialize JSON data of type {!gollum_event}. *)

val write_git_ref :
  Bi_outbuf.t -> git_ref -> unit
  (** Output a JSON value of type {!git_ref}. *)

val string_of_git_ref :
  ?len:int -> git_ref -> string
  (** Serialize a value of type {!git_ref}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_git_ref :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> git_ref
  (** Input JSON data of type {!git_ref}. *)

val git_ref_of_string :
  string -> git_ref
  (** Deserialize JSON data of type {!git_ref}. *)

val write_git_refs :
  Bi_outbuf.t -> git_refs -> unit
  (** Output a JSON value of type {!git_refs}. *)

val string_of_git_refs :
  ?len:int -> git_refs -> string
  (** Serialize a value of type {!git_refs}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_git_refs :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> git_refs
  (** Input JSON data of type {!git_refs}. *)

val git_refs_of_string :
  string -> git_refs
  (** Deserialize JSON data of type {!git_refs}. *)

val write_gist_fork :
  Bi_outbuf.t -> gist_fork -> unit
  (** Output a JSON value of type {!gist_fork}. *)

val string_of_gist_fork :
  ?len:int -> gist_fork -> string
  (** Serialize a value of type {!gist_fork}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_gist_fork :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> gist_fork
  (** Input JSON data of type {!gist_fork}. *)

val gist_fork_of_string :
  string -> gist_fork
  (** Deserialize JSON data of type {!gist_fork}. *)

val write_gist_file :
  Bi_outbuf.t -> gist_file -> unit
  (** Output a JSON value of type {!gist_file}. *)

val string_of_gist_file :
  ?len:int -> gist_file -> string
  (** Serialize a value of type {!gist_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_gist_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> gist_file
  (** Input JSON data of type {!gist_file}. *)

val gist_file_of_string :
  string -> gist_file
  (** Deserialize JSON data of type {!gist_file}. *)

val write_gist_files :
  Bi_outbuf.t -> gist_files -> unit
  (** Output a JSON value of type {!gist_files}. *)

val string_of_gist_files :
  ?len:int -> gist_files -> string
  (** Serialize a value of type {!gist_files}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_gist_files :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> gist_files
  (** Input JSON data of type {!gist_files}. *)

val gist_files_of_string :
  string -> gist_files
  (** Deserialize JSON data of type {!gist_files}. *)

val write_change_status :
  Bi_outbuf.t -> change_status -> unit
  (** Output a JSON value of type {!change_status}. *)

val string_of_change_status :
  ?len:int -> change_status -> string
  (** Serialize a value of type {!change_status}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_change_status :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> change_status
  (** Input JSON data of type {!change_status}. *)

val change_status_of_string :
  string -> change_status
  (** Deserialize JSON data of type {!change_status}. *)

val write_gist_commit :
  Bi_outbuf.t -> gist_commit -> unit
  (** Output a JSON value of type {!gist_commit}. *)

val string_of_gist_commit :
  ?len:int -> gist_commit -> string
  (** Serialize a value of type {!gist_commit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_gist_commit :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> gist_commit
  (** Input JSON data of type {!gist_commit}. *)

val gist_commit_of_string :
  string -> gist_commit
  (** Deserialize JSON data of type {!gist_commit}. *)

val write_gist_commits :
  Bi_outbuf.t -> gist_commits -> unit
  (** Output a JSON value of type {!gist_commits}. *)

val string_of_gist_commits :
  ?len:int -> gist_commits -> string
  (** Serialize a value of type {!gist_commits}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_gist_commits :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> gist_commits
  (** Input JSON data of type {!gist_commits}. *)

val gist_commits_of_string :
  string -> gist_commits
  (** Deserialize JSON data of type {!gist_commits}. *)

val write_gist :
  Bi_outbuf.t -> gist -> unit
  (** Output a JSON value of type {!gist}. *)

val string_of_gist :
  ?len:int -> gist -> string
  (** Serialize a value of type {!gist}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_gist :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> gist
  (** Input JSON data of type {!gist}. *)

val gist_of_string :
  string -> gist
  (** Deserialize JSON data of type {!gist}. *)

val write_gists :
  Bi_outbuf.t -> gists -> unit
  (** Output a JSON value of type {!gists}. *)

val string_of_gists :
  ?len:int -> gists -> string
  (** Serialize a value of type {!gists}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_gists :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> gists
  (** Input JSON data of type {!gists}. *)

val gists_of_string :
  string -> gists
  (** Deserialize JSON data of type {!gists}. *)

val write_gist_forks :
  Bi_outbuf.t -> gist_forks -> unit
  (** Output a JSON value of type {!gist_forks}. *)

val string_of_gist_forks :
  ?len:int -> gist_forks -> string
  (** Serialize a value of type {!gist_forks}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_gist_forks :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> gist_forks
  (** Input JSON data of type {!gist_forks}. *)

val gist_forks_of_string :
  string -> gist_forks
  (** Deserialize JSON data of type {!gist_forks}. *)

val write_fork_event :
  Bi_outbuf.t -> fork_event -> unit
  (** Output a JSON value of type {!fork_event}. *)

val string_of_fork_event :
  ?len:int -> fork_event -> string
  (** Serialize a value of type {!fork_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_fork_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> fork_event
  (** Input JSON data of type {!fork_event}. *)

val fork_event_of_string :
  string -> fork_event
  (** Deserialize JSON data of type {!fork_event}. *)

val write_file :
  Bi_outbuf.t -> file -> unit
  (** Output a JSON value of type {!file}. *)

val string_of_file :
  ?len:int -> file -> string
  (** Serialize a value of type {!file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> file
  (** Input JSON data of type {!file}. *)

val file_of_string :
  string -> file
  (** Deserialize JSON data of type {!file}. *)

val write_files :
  Bi_outbuf.t -> files -> unit
  (** Output a JSON value of type {!files}. *)

val string_of_files :
  ?len:int -> files -> string
  (** Serialize a value of type {!files}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_files :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> files
  (** Input JSON data of type {!files}. *)

val files_of_string :
  string -> files
  (** Deserialize JSON data of type {!files}. *)

val write_delete_event :
  Bi_outbuf.t -> delete_event -> unit
  (** Output a JSON value of type {!delete_event}. *)

val string_of_delete_event :
  ?len:int -> delete_event -> string
  (** Serialize a value of type {!delete_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_delete_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> delete_event
  (** Input JSON data of type {!delete_event}. *)

val delete_event_of_string :
  string -> delete_event
  (** Deserialize JSON data of type {!delete_event}. *)

val write_create_event :
  Bi_outbuf.t -> create_event -> unit
  (** Output a JSON value of type {!create_event}. *)

val string_of_create_event :
  ?len:int -> create_event -> string
  (** Serialize a value of type {!create_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_create_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> create_event
  (** Input JSON data of type {!create_event}. *)

val create_event_of_string :
  string -> create_event
  (** Deserialize JSON data of type {!create_event}. *)

val write_commit_comment :
  Bi_outbuf.t -> commit_comment -> unit
  (** Output a JSON value of type {!commit_comment}. *)

val string_of_commit_comment :
  ?len:int -> commit_comment -> string
  (** Serialize a value of type {!commit_comment}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_commit_comment :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> commit_comment
  (** Input JSON data of type {!commit_comment}. *)

val commit_comment_of_string :
  string -> commit_comment
  (** Deserialize JSON data of type {!commit_comment}. *)

val write_commit_comment_event :
  Bi_outbuf.t -> commit_comment_event -> unit
  (** Output a JSON value of type {!commit_comment_event}. *)

val string_of_commit_comment_event :
  ?len:int -> commit_comment_event -> string
  (** Serialize a value of type {!commit_comment_event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_commit_comment_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> commit_comment_event
  (** Input JSON data of type {!commit_comment_event}. *)

val commit_comment_event_of_string :
  string -> commit_comment_event
  (** Deserialize JSON data of type {!commit_comment_event}. *)

val write_event_constr :
  Bi_outbuf.t -> event_constr -> unit
  (** Output a JSON value of type {!event_constr}. *)

val string_of_event_constr :
  ?len:int -> event_constr -> string
  (** Serialize a value of type {!event_constr}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_event_constr :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> event_constr
  (** Input JSON data of type {!event_constr}. *)

val event_constr_of_string :
  string -> event_constr
  (** Deserialize JSON data of type {!event_constr}. *)

val write_event :
  Bi_outbuf.t -> event -> unit
  (** Output a JSON value of type {!event}. *)

val string_of_event :
  ?len:int -> event -> string
  (** Serialize a value of type {!event}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_event :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> event
  (** Input JSON data of type {!event}. *)

val event_of_string :
  string -> event
  (** Deserialize JSON data of type {!event}. *)

val write_events :
  Bi_outbuf.t -> events -> unit
  (** Output a JSON value of type {!events}. *)

val string_of_events :
  ?len:int -> events -> string
  (** Serialize a value of type {!events}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_events :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> events
  (** Input JSON data of type {!events}. *)

val events_of_string :
  string -> events
  (** Deserialize JSON data of type {!events}. *)

val write_event_hook_metadata :
  Bi_outbuf.t -> event_hook_metadata -> unit
  (** Output a JSON value of type {!event_hook_metadata}. *)

val string_of_event_hook_metadata :
  ?len:int -> event_hook_metadata -> string
  (** Serialize a value of type {!event_hook_metadata}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_event_hook_metadata :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> event_hook_metadata
  (** Input JSON data of type {!event_hook_metadata}. *)

val event_hook_metadata_of_string :
  string -> event_hook_metadata
  (** Deserialize JSON data of type {!event_hook_metadata}. *)

val write_event_hook_constr :
  Bi_outbuf.t -> event_hook_constr -> unit
  (** Output a JSON value of type {!event_hook_constr}. *)

val string_of_event_hook_constr :
  ?len:int -> event_hook_constr -> string
  (** Serialize a value of type {!event_hook_constr}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_event_hook_constr :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> event_hook_constr
  (** Input JSON data of type {!event_hook_constr}. *)

val event_hook_constr_of_string :
  string -> event_hook_constr
  (** Deserialize JSON data of type {!event_hook_constr}. *)

val write_emojis :
  Bi_outbuf.t -> emojis -> unit
  (** Output a JSON value of type {!emojis}. *)

val string_of_emojis :
  ?len:int -> emojis -> string
  (** Serialize a value of type {!emojis}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_emojis :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> emojis
  (** Input JSON data of type {!emojis}. *)

val emojis_of_string :
  string -> emojis
  (** Deserialize JSON data of type {!emojis}. *)

val write_deploy_key :
  Bi_outbuf.t -> deploy_key -> unit
  (** Output a JSON value of type {!deploy_key}. *)

val string_of_deploy_key :
  ?len:int -> deploy_key -> string
  (** Serialize a value of type {!deploy_key}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_deploy_key :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> deploy_key
  (** Input JSON data of type {!deploy_key}. *)

val deploy_key_of_string :
  string -> deploy_key
  (** Deserialize JSON data of type {!deploy_key}. *)

val write_deploy_keys :
  Bi_outbuf.t -> deploy_keys -> unit
  (** Output a JSON value of type {!deploy_keys}. *)

val string_of_deploy_keys :
  ?len:int -> deploy_keys -> string
  (** Serialize a value of type {!deploy_keys}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_deploy_keys :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> deploy_keys
  (** Input JSON data of type {!deploy_keys}. *)

val deploy_keys_of_string :
  string -> deploy_keys
  (** Deserialize JSON data of type {!deploy_keys}. *)

val write_contribution_week :
  Bi_outbuf.t -> contribution_week -> unit
  (** Output a JSON value of type {!contribution_week}. *)

val string_of_contribution_week :
  ?len:int -> contribution_week -> string
  (** Serialize a value of type {!contribution_week}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_contribution_week :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> contribution_week
  (** Input JSON data of type {!contribution_week}. *)

val contribution_week_of_string :
  string -> contribution_week
  (** Deserialize JSON data of type {!contribution_week}. *)

val write_contributor_stats :
  Bi_outbuf.t -> contributor_stats -> unit
  (** Output a JSON value of type {!contributor_stats}. *)

val string_of_contributor_stats :
  ?len:int -> contributor_stats -> string
  (** Serialize a value of type {!contributor_stats}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_contributor_stats :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> contributor_stats
  (** Input JSON data of type {!contributor_stats}. *)

val contributor_stats_of_string :
  string -> contributor_stats
  (** Deserialize JSON data of type {!contributor_stats}. *)

val write_contributors_stats :
  Bi_outbuf.t -> contributors_stats -> unit
  (** Output a JSON value of type {!contributors_stats}. *)

val string_of_contributors_stats :
  ?len:int -> contributors_stats -> string
  (** Serialize a value of type {!contributors_stats}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_contributors_stats :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> contributors_stats
  (** Input JSON data of type {!contributors_stats}. *)

val contributors_stats_of_string :
  string -> contributors_stats
  (** Deserialize JSON data of type {!contributors_stats}. *)

val write_contributor :
  Bi_outbuf.t -> contributor -> unit
  (** Output a JSON value of type {!contributor}. *)

val string_of_contributor :
  ?len:int -> contributor -> string
  (** Serialize a value of type {!contributor}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_contributor :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> contributor
  (** Input JSON data of type {!contributor}. *)

val contributor_of_string :
  string -> contributor
  (** Deserialize JSON data of type {!contributor}. *)

val write_contributors :
  Bi_outbuf.t -> contributors -> unit
  (** Output a JSON value of type {!contributors}. *)

val string_of_contributors :
  ?len:int -> contributors -> string
  (** Serialize a value of type {!contributors}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_contributors :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> contributors
  (** Input JSON data of type {!contributors}. *)

val contributors_of_string :
  string -> contributors
  (** Deserialize JSON data of type {!contributors}. *)

val write_commits :
  Bi_outbuf.t -> commits -> unit
  (** Output a JSON value of type {!commits}. *)

val string_of_commits :
  ?len:int -> commits -> string
  (** Serialize a value of type {!commits}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_commits :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> commits
  (** Input JSON data of type {!commits}. *)

val commits_of_string :
  string -> commits
  (** Deserialize JSON data of type {!commits}. *)

val write_comment :
  Bi_outbuf.t -> comment -> unit
  (** Output a JSON value of type {!comment}. *)

val string_of_comment :
  ?len:int -> comment -> string
  (** Serialize a value of type {!comment}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_comment :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> comment
  (** Input JSON data of type {!comment}. *)

val comment_of_string :
  string -> comment
  (** Deserialize JSON data of type {!comment}. *)

val write_base_status :
  Bi_outbuf.t -> base_status -> unit
  (** Output a JSON value of type {!base_status}. *)

val string_of_base_status :
  ?len:int -> base_status -> string
  (** Serialize a value of type {!base_status}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_base_status :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> base_status
  (** Input JSON data of type {!base_status}. *)

val base_status_of_string :
  string -> base_status
  (** Deserialize JSON data of type {!base_status}. *)

val write_base_statuses :
  Bi_outbuf.t -> base_statuses -> unit
  (** Output a JSON value of type {!base_statuses}. *)

val string_of_base_statuses :
  ?len:int -> base_statuses -> string
  (** Serialize a value of type {!base_statuses}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_base_statuses :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> base_statuses
  (** Input JSON data of type {!base_statuses}. *)

val base_statuses_of_string :
  string -> base_statuses
  (** Deserialize JSON data of type {!base_statuses}. *)

val write_combined_status :
  Bi_outbuf.t -> combined_status -> unit
  (** Output a JSON value of type {!combined_status}. *)

val string_of_combined_status :
  ?len:int -> combined_status -> string
  (** Serialize a value of type {!combined_status}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_combined_status :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> combined_status
  (** Input JSON data of type {!combined_status}. *)

val combined_status_of_string :
  string -> combined_status
  (** Deserialize JSON data of type {!combined_status}. *)

val write_app :
  Bi_outbuf.t -> app -> unit
  (** Output a JSON value of type {!app}. *)

val string_of_app :
  ?len:int -> app -> string
  (** Serialize a value of type {!app}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_app :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> app
  (** Input JSON data of type {!app}. *)

val app_of_string :
  string -> app
  (** Deserialize JSON data of type {!app}. *)

val write_auth :
  Bi_outbuf.t -> auth -> unit
  (** Output a JSON value of type {!auth}. *)

val string_of_auth :
  ?len:int -> auth -> string
  (** Serialize a value of type {!auth}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_auth :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> auth
  (** Input JSON data of type {!auth}. *)

val auth_of_string :
  string -> auth
  (** Deserialize JSON data of type {!auth}. *)

val write_auths :
  Bi_outbuf.t -> auths -> unit
  (** Output a JSON value of type {!auths}. *)

val string_of_auths :
  ?len:int -> auths -> string
  (** Serialize a value of type {!auths}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_auths :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> auths
  (** Input JSON data of type {!auths}. *)

val auths_of_string :
  string -> auths
  (** Deserialize JSON data of type {!auths}. *)

val write_auth_req :
  Bi_outbuf.t -> auth_req -> unit
  (** Output a JSON value of type {!auth_req}. *)

val string_of_auth_req :
  ?len:int -> auth_req -> string
  (** Serialize a value of type {!auth_req}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_auth_req :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> auth_req
  (** Input JSON data of type {!auth_req}. *)

val auth_req_of_string :
  string -> auth_req
  (** Deserialize JSON data of type {!auth_req}. *)

